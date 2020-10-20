
	#' List QuestionAnswerMedias
	#'
	#' This function returns a dataframe or json object of QuestionAnswerMedias
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('QuestionAnswerMedia') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of QuestionAnswerMedias
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listQuestionAnswerMedias <- function(searchConditionsList = NULL, QuestionAnswerMediaID = F, QuestionAnswerID = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayFor = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "QuestionAnswerMedia", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get QuestionAnswerMedia
	#'
	#' This function returns a dataframe or json object of a QuestionAnswerMedia
	#' @param QuestionAnswerMediaID The ID of the QuestionAnswerMedia to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('QuestionAnswerMedia') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getQuestionAnswerMedia
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getQuestionAnswerMedia <- function(QuestionAnswerMediaID, QuestionAnswerID = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayFor = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "QuestionAnswerMediaID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "QuestionAnswerMedia", objectId = QuestionAnswerMediaID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List QuestionMedias
	#'
	#' This function returns a dataframe or json object of QuestionMedias
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('QuestionMedia') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of QuestionMedias
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listQuestionMedias <- function(searchConditionsList = NULL, QuestionMediaID = F, QuestionID = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "QuestionMedia", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get QuestionMedia
	#'
	#' This function returns a dataframe or json object of a QuestionMedia
	#' @param QuestionMediaID The ID of the QuestionMedia to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('QuestionMedia') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getQuestionMedia
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getQuestionMedia <- function(QuestionMediaID, QuestionID = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "QuestionMediaID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "QuestionMedia", objectId = QuestionMediaID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAnswers
	#'
	#' This function returns a dataframe or json object of StudentAnswers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAnswer') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StudentAnswers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAnswers <- function(searchConditionsList = NULL, StudentAnswerID = F, StudentQuestionID = F, QuestionAnswerID = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentAnswer", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentAnswer
	#'
	#' This function returns a dataframe or json object of a StudentAnswer
	#' @param StudentAnswerID The ID of the StudentAnswer to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAnswer') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStudentAnswer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAnswer <- function(StudentAnswerID, StudentQuestionID = F, QuestionAnswerID = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAnswerID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StudentAnswer", objectId = StudentAnswerID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentQuestions
	#'
	#' This function returns a dataframe or json object of StudentQuestions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentQuestion') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StudentQuestions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentQuestions <- function(searchConditionsList = NULL, StudentQuestionID = F, StudentAssignmentID = F, AssignmentQuestionID = F, Score = F, Order = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentQuestion", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentQuestion
	#'
	#' This function returns a dataframe or json object of a StudentQuestion
	#' @param StudentQuestionID The ID of the StudentQuestion to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentQuestion') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStudentQuestion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentQuestion <- function(StudentQuestionID, StudentAssignmentID = F, AssignmentQuestionID = F, Score = F, Order = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentQuestionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StudentQuestion", objectId = StudentQuestionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentQuestions
	#'
	#' This function returns a dataframe or json object of AssignmentQuestions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AssignmentQuestion') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of AssignmentQuestions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentQuestions <- function(searchConditionsList = NULL, AssignmentQuestionID = F, AssignmentID = F, QuestionID = F, PointsAllowed = F, AllowPartialCredit = F, DisplayOrder = F, QuestionAnswerCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "AssignmentQuestion", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get AssignmentQuestion
	#'
	#' This function returns a dataframe or json object of an AssignmentQuestion
	#' @param AssignmentQuestionID The ID of the AssignmentQuestion to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AssignmentQuestion') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getAssignmentQuestion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentQuestion <- function(AssignmentQuestionID, AssignmentID = F, QuestionID = F, PointsAllowed = F, AllowPartialCredit = F, DisplayOrder = F, QuestionAnswerCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentQuestionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "AssignmentQuestion", objectId = AssignmentQuestionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ClassGroupSections
	#'
	#' This function returns a dataframe or json object of ClassGroupSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ClassGroupSection') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of ClassGroupSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listClassGroupSections <- function(searchConditionsList = NULL, ClassGroupSectionID = F, ClassGroupID = F, SectionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ClassGroupSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get ClassGroupSection
	#'
	#' This function returns a dataframe or json object of a ClassGroupSection
	#' @param ClassGroupSectionID The ID of the ClassGroupSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ClassGroupSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getClassGroupSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getClassGroupSection <- function(ClassGroupSectionID, ClassGroupID = F, SectionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ClassGroupSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "ClassGroupSection", objectId = ClassGroupSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Questions
	#'
	#' This function returns a dataframe or json object of Questions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Question') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of Questions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listQuestions <- function(searchConditionsList = NULL, QuestionID = F, StaffID = F, Description = F, Instructions = F, Type = F, AllowStudentAttachments = F, UseAnswers = F, IsShortAnswer = F, IsEssay = F, IsMatching = F, IsMultipleChoice = F, IsTrueFalse = F, HasAutoScoredAssignment = F, HasAssignmentPastEndTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentCount = F, AssignmentCount = F, HasMultipleAssignments = F, HasQuestionMedias = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "Question", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get Question
	#'
	#' This function returns a dataframe or json object of a Question
	#' @param QuestionID The ID of the Question to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Question') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getQuestion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getQuestion <- function(QuestionID, StaffID = F, Description = F, Instructions = F, Type = F, AllowStudentAttachments = F, UseAnswers = F, IsShortAnswer = F, IsEssay = F, IsMatching = F, IsMultipleChoice = F, IsTrueFalse = F, HasAutoScoredAssignment = F, HasAssignmentPastEndTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentCount = F, AssignmentCount = F, HasMultipleAssignments = F, HasQuestionMedias = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "QuestionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "Question", objectId = QuestionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List QuestionAnswers
	#'
	#' This function returns a dataframe or json object of QuestionAnswers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('QuestionAnswer') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of QuestionAnswers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listQuestionAnswers <- function(searchConditionsList = NULL, QuestionAnswerID = F, QuestionID = F, IsCorrect = F, Value = F, Choice = F, ChoiceOrder = F, ValueOrder = F, IsShortAnswer = F, IsEssay = F, IsTrueFalse = F, IsMultipleChoice = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasAttachedMedia = F, HasAttachedChoiceMedia = F, HasAttachedMediaSafe = F, HasAttachedChoiceMediaSafe = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "QuestionAnswer", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get QuestionAnswer
	#'
	#' This function returns a dataframe or json object of a QuestionAnswer
	#' @param QuestionAnswerID The ID of the QuestionAnswer to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('QuestionAnswer') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getQuestionAnswer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getQuestionAnswer <- function(QuestionAnswerID, QuestionID = F, IsCorrect = F, Value = F, Choice = F, ChoiceOrder = F, ValueOrder = F, IsShortAnswer = F, IsEssay = F, IsTrueFalse = F, IsMultipleChoice = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasAttachedMedia = F, HasAttachedChoiceMedia = F, HasAttachedMediaSafe = F, HasAttachedChoiceMediaSafe = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "QuestionAnswerID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "QuestionAnswer", objectId = QuestionAnswerID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalculationGroupSubjectAcademicStandards
	#'
	#' This function returns a dataframe or json object of CalculationGroupSubjectAcademicStandards
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupSubjectAcademicStandard') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CalculationGroupSubjectAcademicStandards
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalculationGroupSubjectAcademicStandards <- function(searchConditionsList = NULL, CalculationGroupSubjectAcademicStandardID = F, EntityGroupKey = F, CalculationGroupID = F, SubjectID = F, AcademicStandardID = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupSubjectAcademicStandardIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupSubjectAcademicStandard", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CalculationGroupSubjectAcademicStandard
	#'
	#' This function returns a dataframe or json object of a CalculationGroupSubjectAcademicStandard
	#' @param CalculationGroupSubjectAcademicStandardID The ID of the CalculationGroupSubjectAcademicStandard to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupSubjectAcademicStandard') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCalculationGroupSubjectAcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalculationGroupSubjectAcademicStandard <- function(CalculationGroupSubjectAcademicStandardID, EntityGroupKey = F, CalculationGroupID = F, SubjectID = F, AcademicStandardID = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupSubjectAcademicStandardIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalculationGroupSubjectAcademicStandardID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CalculationGroupSubjectAcademicStandard", objectId = CalculationGroupSubjectAcademicStandardID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ClassGroups
	#'
	#' This function returns a dataframe or json object of ClassGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ClassGroup') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of ClassGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listClassGroups <- function(searchConditionsList = NULL, ClassGroupID = F, Name = F, EntityID = F, SchoolYearID = F, StaffID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAHistoricRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ClassGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get ClassGroup
	#'
	#' This function returns a dataframe or json object of a ClassGroup
	#' @param ClassGroupID The ID of the ClassGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ClassGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getClassGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getClassGroup <- function(ClassGroupID, Name = F, EntityID = F, SchoolYearID = F, StaffID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAHistoricRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ClassGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "ClassGroup", objectId = ClassGroupID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionAcademicStandardWeights
	#'
	#' This function returns a dataframe or json object of SectionAcademicStandardWeights
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionAcademicStandardWeight') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of SectionAcademicStandardWeights
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionAcademicStandardWeights <- function(searchConditionsList = NULL, SectionAcademicStandardWeightID = F, SectionGradeBucketID = F, AcademicStandardID = F, WeightPercent = F, HasAssignments = F, AdjustedWeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionAcademicStandardWeight", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionAcademicStandardWeight
	#'
	#' This function returns a dataframe or json object of a SectionAcademicStandardWeight
	#' @param SectionAcademicStandardWeightID The ID of the SectionAcademicStandardWeight to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionAcademicStandardWeight') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getSectionAcademicStandardWeight
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionAcademicStandardWeight <- function(SectionAcademicStandardWeightID, SectionGradeBucketID = F, AcademicStandardID = F, WeightPercent = F, HasAssignments = F, AdjustedWeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionAcademicStandardWeightID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "SectionAcademicStandardWeight", objectId = SectionAcademicStandardWeightID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AcademicStandardGradingScaleGroups
	#'
	#' This function returns a dataframe or json object of AcademicStandardGradingScaleGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AcademicStandardGradingScaleGroup') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of AcademicStandardGradingScaleGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAcademicStandardGradingScaleGroups <- function(searchConditionsList = NULL, AcademicStandardGradingScaleGroupID = F, GradingScaleGroupID = F, CalculationGroupID = F, Description = F, EntityGroupKey = F, IsDefaultGroup = F, IsAHistoricRecord = F, StandardCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AcademicStandardGradingScaleGroupIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "AcademicStandardGradingScaleGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get AcademicStandardGradingScaleGroup
	#'
	#' This function returns a dataframe or json object of an AcademicStandardGradingScaleGroup
	#' @param AcademicStandardGradingScaleGroupID The ID of the AcademicStandardGradingScaleGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AcademicStandardGradingScaleGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getAcademicStandardGradingScaleGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAcademicStandardGradingScaleGroup <- function(AcademicStandardGradingScaleGroupID, GradingScaleGroupID = F, CalculationGroupID = F, Description = F, EntityGroupKey = F, IsDefaultGroup = F, IsAHistoricRecord = F, StandardCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AcademicStandardGradingScaleGroupIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AcademicStandardGradingScaleGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "AcademicStandardGradingScaleGroup", objectId = AcademicStandardGradingScaleGroupID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AcademicStandardGradingScaleGroupAcademicStandards
	#'
	#' This function returns a dataframe or json object of AcademicStandardGradingScaleGroupAcademicStandards
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AcademicStandardGradingScaleGroupAcademicStandard') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of AcademicStandardGradingScaleGroupAcademicStandards
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAcademicStandardGradingScaleGroupAcademicStandards <- function(searchConditionsList = NULL, AcademicStandardGradingScaleGroupAcademicStandardID = F, AcademicStandardGradingScaleGroupID = F, CalculationGroupAcademicStandardID = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AcademicStandardGradingScaleGroupAcademicStandardIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "AcademicStandardGradingScaleGroupAcademicStandard", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get AcademicStandardGradingScaleGroupAcademicStandard
	#'
	#' This function returns a dataframe or json object of an AcademicStandardGradingScaleGroupAcademicStandard
	#' @param AcademicStandardGradingScaleGroupAcademicStandardID The ID of the AcademicStandardGradingScaleGroupAcademicStandard to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AcademicStandardGradingScaleGroupAcademicStandard') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getAcademicStandardGradingScaleGroupAcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAcademicStandardGradingScaleGroupAcademicStandard <- function(AcademicStandardGradingScaleGroupAcademicStandardID, AcademicStandardGradingScaleGroupID = F, CalculationGroupAcademicStandardID = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AcademicStandardGradingScaleGroupAcademicStandardIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AcademicStandardGradingScaleGroupAcademicStandardID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "AcademicStandardGradingScaleGroupAcademicStandard", objectId = AcademicStandardGradingScaleGroupAcademicStandardID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSubjectWeights
	#'
	#' This function returns a dataframe or json object of SectionSubjectWeights
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSubjectWeight') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of SectionSubjectWeights
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSubjectWeights <- function(searchConditionsList = NULL, SectionSubjectWeightID = F, SectionGradeBucketID = F, SubjectID = F, WeightPercent = F, HasAssignments = F, AdjustedWeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionSubjectWeight", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSubjectWeight
	#'
	#' This function returns a dataframe or json object of a SectionSubjectWeight
	#' @param SectionSubjectWeightID The ID of the SectionSubjectWeight to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSubjectWeight') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getSectionSubjectWeight
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSubjectWeight <- function(SectionSubjectWeightID, SectionGradeBucketID = F, SubjectID = F, WeightPercent = F, HasAssignments = F, AdjustedWeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSubjectWeightID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "SectionSubjectWeight", objectId = SectionSubjectWeightID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalculationGroupAcademicStandardGradeBuckets
	#'
	#' This function returns a dataframe or json object of CalculationGroupAcademicStandardGradeBuckets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupAcademicStandardGradeBucket') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CalculationGroupAcademicStandardGradeBuckets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalculationGroupAcademicStandardGradeBuckets <- function(searchConditionsList = NULL, CalculationGroupAcademicStandardGradeBucketID = F, CalculationGroupAcademicStandardID = F, CalculationGroupGradeBucketID = F, CalculationType = F, EntityGroupKey = F, IsAHistoricRecord = F, HasCategoryWeighting = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasAssignments = F, IsWeightedOnByACalculationGroupSubjectGradeBucket = F, IsWeightedOnInASectionOverride = F, CalculationGroupAcademicStandardGradeBucketIDClonedFrom = F, HasCalculationGroupWeight = F, HasCalculationGroupSubjectWeight = F, HasAcademicStandardWeighting = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupAcademicStandardGradeBucket", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CalculationGroupAcademicStandardGradeBucket
	#'
	#' This function returns a dataframe or json object of a CalculationGroupAcademicStandardGradeBucket
	#' @param CalculationGroupAcademicStandardGradeBucketID The ID of the CalculationGroupAcademicStandardGradeBucket to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupAcademicStandardGradeBucket') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCalculationGroupAcademicStandardGradeBucket
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalculationGroupAcademicStandardGradeBucket <- function(CalculationGroupAcademicStandardGradeBucketID, CalculationGroupAcademicStandardID = F, CalculationGroupGradeBucketID = F, CalculationType = F, EntityGroupKey = F, IsAHistoricRecord = F, HasCategoryWeighting = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasAssignments = F, IsWeightedOnByACalculationGroupSubjectGradeBucket = F, IsWeightedOnInASectionOverride = F, CalculationGroupAcademicStandardGradeBucketIDClonedFrom = F, HasCalculationGroupWeight = F, HasCalculationGroupSubjectWeight = F, HasAcademicStandardWeighting = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalculationGroupAcademicStandardGradeBucketID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CalculationGroupAcademicStandardGradeBucket", objectId = CalculationGroupAcademicStandardGradeBucketID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalculationGroupAcademicStandardWeightings
	#'
	#' This function returns a dataframe or json object of CalculationGroupAcademicStandardWeightings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupAcademicStandardWeighting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CalculationGroupAcademicStandardWeightings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalculationGroupAcademicStandardWeightings <- function(searchConditionsList = NULL, CalculationGroupAcademicStandardWeightingID = F, CalculationGroupAcademicStandardGradeBucketID = F, CategoryIDToWeight = F, WeightPercent = F, IsAHistoricRecord = F, AdjustedWeightPercent = F, HasAssignments = F, PercentEarnedCategory = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityGroupKey = F, CalculationGroupAcademicStandardWeightingIDClonedFrom = F, AcademicStandardIDToWeight = F, UseForWeightSum = F, PointsEarnedCategory = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupAcademicStandardWeighting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CalculationGroupAcademicStandardWeighting
	#'
	#' This function returns a dataframe or json object of a CalculationGroupAcademicStandardWeighting
	#' @param CalculationGroupAcademicStandardWeightingID The ID of the CalculationGroupAcademicStandardWeighting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupAcademicStandardWeighting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCalculationGroupAcademicStandardWeighting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalculationGroupAcademicStandardWeighting <- function(CalculationGroupAcademicStandardWeightingID, CalculationGroupAcademicStandardGradeBucketID = F, CategoryIDToWeight = F, WeightPercent = F, IsAHistoricRecord = F, AdjustedWeightPercent = F, HasAssignments = F, PercentEarnedCategory = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityGroupKey = F, CalculationGroupAcademicStandardWeightingIDClonedFrom = F, AcademicStandardIDToWeight = F, UseForWeightSum = F, PointsEarnedCategory = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalculationGroupAcademicStandardWeightingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CalculationGroupAcademicStandardWeighting", objectId = CalculationGroupAcademicStandardWeightingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCalculationGroupAcademicStandardWeightings
	#'
	#' This function returns a dataframe or json object of TempCalculationGroupAcademicStandardWeightings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCalculationGroupAcademicStandardWeighting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempCalculationGroupAcademicStandardWeightings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCalculationGroupAcademicStandardWeightings <- function(searchConditionsList = NULL, TempCalculationGroupAcademicStandardWeightingID = F, CalculationGroupAcademicStandardGradeBucketID = F, CategoryID = F, CategoryDescription = F, WeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCalculationGroupAcademicStandardWeighting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempCalculationGroupAcademicStandardWeighting
	#'
	#' This function returns a dataframe or json object of a TempCalculationGroupAcademicStandardWeighting
	#' @param TempCalculationGroupAcademicStandardWeightingID The ID of the TempCalculationGroupAcademicStandardWeighting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCalculationGroupAcademicStandardWeighting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempCalculationGroupAcademicStandardWeighting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCalculationGroupAcademicStandardWeighting <- function(TempCalculationGroupAcademicStandardWeightingID, CalculationGroupAcademicStandardGradeBucketID = F, CategoryID = F, CategoryDescription = F, WeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCalculationGroupAcademicStandardWeightingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempCalculationGroupAcademicStandardWeighting", objectId = TempCalculationGroupAcademicStandardWeightingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalculationGroupAcademicStandards
	#'
	#' This function returns a dataframe or json object of CalculationGroupAcademicStandards
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupAcademicStandard') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CalculationGroupAcademicStandards
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalculationGroupAcademicStandards <- function(searchConditionsList = NULL, CalculationGroupAcademicStandardID = F, CalculationGroupID = F, AcademicStandardID = F, EntityGroupKey = F, HasParentSubject = F, HasGradingScale = F, HasAssignments = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupAcademicStandardIDClonedFrom = F, Order = F, Level = F, CalculationGroupAcademicStandardIDParent = F, DisplayOnReportCard = F, CalculateForSingleBucket = F, IsDescendantOfSpecifiedAcademicStandard = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupAcademicStandard", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CalculationGroupAcademicStandard
	#'
	#' This function returns a dataframe or json object of a CalculationGroupAcademicStandard
	#' @param CalculationGroupAcademicStandardID The ID of the CalculationGroupAcademicStandard to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupAcademicStandard') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCalculationGroupAcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalculationGroupAcademicStandard <- function(CalculationGroupAcademicStandardID, CalculationGroupID = F, AcademicStandardID = F, EntityGroupKey = F, HasParentSubject = F, HasGradingScale = F, HasAssignments = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupAcademicStandardIDClonedFrom = F, Order = F, Level = F, CalculationGroupAcademicStandardIDParent = F, DisplayOnReportCard = F, CalculateForSingleBucket = F, IsDescendantOfSpecifiedAcademicStandard = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalculationGroupAcademicStandardID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CalculationGroupAcademicStandard", objectId = CalculationGroupAcademicStandardID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalculationGroupSubjects
	#'
	#' This function returns a dataframe or json object of CalculationGroupSubjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupSubject') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CalculationGroupSubjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalculationGroupSubjects <- function(searchConditionsList = NULL, CalculationGroupSubjectID = F, CalculationGroupID = F, SubjectID = F, EntityGroupKey = F, IsAHistoricRecord = F, HasGradingScale = F, HasAssignments = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupSubjectIDClonedFrom = F, Order = F, TopLevelAcademicStandardHierarchyDepthDescription = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupSubject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CalculationGroupSubject
	#'
	#' This function returns a dataframe or json object of a CalculationGroupSubject
	#' @param CalculationGroupSubjectID The ID of the CalculationGroupSubject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupSubject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCalculationGroupSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalculationGroupSubject <- function(CalculationGroupSubjectID, CalculationGroupID = F, SubjectID = F, EntityGroupKey = F, IsAHistoricRecord = F, HasGradingScale = F, HasAssignments = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupSubjectIDClonedFrom = F, Order = F, TopLevelAcademicStandardHierarchyDepthDescription = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalculationGroupSubjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CalculationGroupSubject", objectId = CalculationGroupSubjectID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalculationGroupSubjectWeightings
	#'
	#' This function returns a dataframe or json object of CalculationGroupSubjectWeightings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupSubjectWeighting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CalculationGroupSubjectWeightings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalculationGroupSubjectWeightings <- function(searchConditionsList = NULL, CalculationGroupSubjectWeightingID = F, CalculationGroupSubjectGradeBucketID = F, CategoryIDToWeight = F, AcademicStandardIDToWeight = F, WeightPercent = F, IsAHistoricRecord = F, AdjustedWeightPercent = F, HasAssignments = F, UseForWeightSum = F, PercentEarnedCategory = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityGroupKey = F, CalculationGroupSubjectWeightingIDClonedFrom = F, PointsEarnedCategory = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupSubjectWeighting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CalculationGroupSubjectWeighting
	#'
	#' This function returns a dataframe or json object of a CalculationGroupSubjectWeighting
	#' @param CalculationGroupSubjectWeightingID The ID of the CalculationGroupSubjectWeighting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupSubjectWeighting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCalculationGroupSubjectWeighting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalculationGroupSubjectWeighting <- function(CalculationGroupSubjectWeightingID, CalculationGroupSubjectGradeBucketID = F, CategoryIDToWeight = F, AcademicStandardIDToWeight = F, WeightPercent = F, IsAHistoricRecord = F, AdjustedWeightPercent = F, HasAssignments = F, UseForWeightSum = F, PercentEarnedCategory = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityGroupKey = F, CalculationGroupSubjectWeightingIDClonedFrom = F, PointsEarnedCategory = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalculationGroupSubjectWeightingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CalculationGroupSubjectWeighting", objectId = CalculationGroupSubjectWeightingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCalculationGroupSubjectWeightings
	#'
	#' This function returns a dataframe or json object of TempCalculationGroupSubjectWeightings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCalculationGroupSubjectWeighting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempCalculationGroupSubjectWeightings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCalculationGroupSubjectWeightings <- function(searchConditionsList = NULL, TempCalculationGroupSubjectWeightingID = F, CalculationGroupSubjectGradeBucketID = F, CategoryID = F, CategoryDescription = F, WeightPercent = F, AcademicStandardID = F, AcademicStandardDescription = F, AcademicStandardFullKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCalculationGroupSubjectWeighting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempCalculationGroupSubjectWeighting
	#'
	#' This function returns a dataframe or json object of a TempCalculationGroupSubjectWeighting
	#' @param TempCalculationGroupSubjectWeightingID The ID of the TempCalculationGroupSubjectWeighting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCalculationGroupSubjectWeighting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempCalculationGroupSubjectWeighting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCalculationGroupSubjectWeighting <- function(TempCalculationGroupSubjectWeightingID, CalculationGroupSubjectGradeBucketID = F, CategoryID = F, CategoryDescription = F, WeightPercent = F, AcademicStandardID = F, AcademicStandardDescription = F, AcademicStandardFullKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCalculationGroupSubjectWeightingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempCalculationGroupSubjectWeighting", objectId = TempCalculationGroupSubjectWeightingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalculationGroupSubjectGradeBuckets
	#'
	#' This function returns a dataframe or json object of CalculationGroupSubjectGradeBuckets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupSubjectGradeBucket') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CalculationGroupSubjectGradeBuckets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalculationGroupSubjectGradeBuckets <- function(searchConditionsList = NULL, CalculationGroupSubjectGradeBucketID = F, CalculationGroupSubjectID = F, CalculationGroupGradeBucketID = F, CalculationType = F, EntityGroupKey = F, IsAHistoricRecord = F, HasAssignments = F, HasCategoryWeighting = F, HasSubjectAcademicStandards = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupSubjectGradeBucketIDClonedFrom = F, HasCalculationGroupWeight = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupSubjectGradeBucket", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CalculationGroupSubjectGradeBucket
	#'
	#' This function returns a dataframe or json object of a CalculationGroupSubjectGradeBucket
	#' @param CalculationGroupSubjectGradeBucketID The ID of the CalculationGroupSubjectGradeBucket to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupSubjectGradeBucket') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCalculationGroupSubjectGradeBucket
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalculationGroupSubjectGradeBucket <- function(CalculationGroupSubjectGradeBucketID, CalculationGroupSubjectID = F, CalculationGroupGradeBucketID = F, CalculationType = F, EntityGroupKey = F, IsAHistoricRecord = F, HasAssignments = F, HasCategoryWeighting = F, HasSubjectAcademicStandards = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupSubjectGradeBucketIDClonedFrom = F, HasCalculationGroupWeight = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalculationGroupSubjectGradeBucketID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CalculationGroupSubjectGradeBucket", objectId = CalculationGroupSubjectGradeBucketID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubjectGradingScaleGroupSubjects
	#'
	#' This function returns a dataframe or json object of SubjectGradingScaleGroupSubjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SubjectGradingScaleGroupSubject') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of SubjectGradingScaleGroupSubjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubjectGradingScaleGroupSubjects <- function(searchConditionsList = NULL, SubjectGradingScaleGroupSubjectID = F, SubjectGradingScaleGroupID = F, CalculationGroupSubjectID = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubjectGradingScaleGroupSubjectIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SubjectGradingScaleGroupSubject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SubjectGradingScaleGroupSubject
	#'
	#' This function returns a dataframe or json object of a SubjectGradingScaleGroupSubject
	#' @param SubjectGradingScaleGroupSubjectID The ID of the SubjectGradingScaleGroupSubject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SubjectGradingScaleGroupSubject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getSubjectGradingScaleGroupSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubjectGradingScaleGroupSubject <- function(SubjectGradingScaleGroupSubjectID, SubjectGradingScaleGroupID = F, CalculationGroupSubjectID = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubjectGradingScaleGroupSubjectIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubjectGradingScaleGroupSubjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "SubjectGradingScaleGroupSubject", objectId = SubjectGradingScaleGroupSubjectID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubjectGradingScaleGroups
	#'
	#' This function returns a dataframe or json object of SubjectGradingScaleGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SubjectGradingScaleGroup') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of SubjectGradingScaleGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubjectGradingScaleGroups <- function(searchConditionsList = NULL, SubjectGradingScaleGroupID = F, GradingScaleGroupID = F, CalculationGroupID = F, Description = F, EntityGroupKey = F, IsDefaultGroup = F, IsAHistoricRecord = F, SubjectCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubjectGradingScaleGroupIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SubjectGradingScaleGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SubjectGradingScaleGroup
	#'
	#' This function returns a dataframe or json object of a SubjectGradingScaleGroup
	#' @param SubjectGradingScaleGroupID The ID of the SubjectGradingScaleGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SubjectGradingScaleGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getSubjectGradingScaleGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubjectGradingScaleGroup <- function(SubjectGradingScaleGroupID, GradingScaleGroupID = F, CalculationGroupID = F, Description = F, EntityGroupKey = F, IsDefaultGroup = F, IsAHistoricRecord = F, SubjectCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubjectGradingScaleGroupIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubjectGradingScaleGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "SubjectGradingScaleGroup", objectId = SubjectGradingScaleGroupID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GradingScaleGroups
	#'
	#' This function returns a dataframe or json object of GradingScaleGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradingScaleGroup') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of GradingScaleGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGradingScaleGroups <- function(searchConditionsList = NULL, GradingScaleGroupID = F, EntityID = F, SchoolYearID = F, Description = F, EntityGroupKey = F, MaxScore = F, GradingScaleType = F, IsDefaultSubjectGradingScaleGroup = F, IsDefaultAcademicStandardGradingScaleGroup = F, IsAHistoricRecord = F, HasSubjectGradingScaleGroups = F, HasAcademicStandardGradingScaleGroups = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayGradeMarkPercentages = F, GradingScaleGroupIDClonedFrom = F, SectionIDTeacherOverride = F, HasCourseGradingScaleGroups = F, HasSectionGradingScaleGroups = F, IsDummySectionContainer = F, IsSectionRelatedGradingScaleGroup = F, HasStudentGradingScaleGroups = F, IsPointsBasedScale = F, UseAsMastery = F, GradeMarkIDMastered = F, GradingScaleGroupExistsInSpecifcEntity = F, HasGradeMarkScoring = F, HasGradeMarksThatHaveChangedPercent = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "GradingScaleGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get GradingScaleGroup
	#'
	#' This function returns a dataframe or json object of a GradingScaleGroup
	#' @param GradingScaleGroupID The ID of the GradingScaleGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradingScaleGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getGradingScaleGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGradingScaleGroup <- function(GradingScaleGroupID, EntityID = F, SchoolYearID = F, Description = F, EntityGroupKey = F, MaxScore = F, GradingScaleType = F, IsDefaultSubjectGradingScaleGroup = F, IsDefaultAcademicStandardGradingScaleGroup = F, IsAHistoricRecord = F, HasSubjectGradingScaleGroups = F, HasAcademicStandardGradingScaleGroups = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayGradeMarkPercentages = F, GradingScaleGroupIDClonedFrom = F, SectionIDTeacherOverride = F, HasCourseGradingScaleGroups = F, HasSectionGradingScaleGroups = F, IsDummySectionContainer = F, IsSectionRelatedGradingScaleGroup = F, HasStudentGradingScaleGroups = F, IsPointsBasedScale = F, UseAsMastery = F, GradeMarkIDMastered = F, GradingScaleGroupExistsInSpecifcEntity = F, HasGradeMarkScoring = F, HasGradeMarksThatHaveChangedPercent = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GradingScaleGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "GradingScaleGroup", objectId = GradingScaleGroupID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentGradeBucketSubjects
	#'
	#' This function returns a dataframe or json object of StudentGradeBucketSubjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGradeBucketSubject') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StudentGradeBucketSubjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentGradeBucketSubjects <- function(searchConditionsList = NULL, StudentGradeBucketSubjectID = F, StudentGradeBucketID = F, SubjectID = F, SectionID = F, GradeMarkID = F, Percent = F, EarnedPoints = F, PossiblePoints = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAdminOverride = F, GradeMarkIDOverride = F, GradeMarkOverrideComment = F, PercentAdjustment = F, PercentAdjustmentComment = F, PercentWithAdjustment = F, GradeMarkToUse = F, PercentWithAdjustmentWithGradeMarkToUse = F, PercentWithGradeMark = F, CalculatedPoints = F, IsUsingPointsBasedScale = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGradeBucketSubject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentGradeBucketSubject
	#'
	#' This function returns a dataframe or json object of a StudentGradeBucketSubject
	#' @param StudentGradeBucketSubjectID The ID of the StudentGradeBucketSubject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGradeBucketSubject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStudentGradeBucketSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentGradeBucketSubject <- function(StudentGradeBucketSubjectID, StudentGradeBucketID = F, SubjectID = F, SectionID = F, GradeMarkID = F, Percent = F, EarnedPoints = F, PossiblePoints = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAdminOverride = F, GradeMarkIDOverride = F, GradeMarkOverrideComment = F, PercentAdjustment = F, PercentAdjustmentComment = F, PercentWithAdjustment = F, GradeMarkToUse = F, PercentWithAdjustmentWithGradeMarkToUse = F, PercentWithGradeMark = F, CalculatedPoints = F, IsUsingPointsBasedScale = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentGradeBucketSubjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StudentGradeBucketSubject", objectId = StudentGradeBucketSubjectID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TeacherSectionStudentSectionSettings
	#'
	#' This function returns a dataframe or json object of TeacherSectionStudentSectionSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionStudentSectionSetting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TeacherSectionStudentSectionSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTeacherSectionStudentSectionSettings <- function(searchConditionsList = NULL, TeacherSectionStudentSectionSettingID = F, TeacherSectionSettingID = F, StudentSectionID = F, DisplayOrder = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionStudentSectionSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TeacherSectionStudentSectionSetting
	#'
	#' This function returns a dataframe or json object of a TeacherSectionStudentSectionSetting
	#' @param TeacherSectionStudentSectionSettingID The ID of the TeacherSectionStudentSectionSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionStudentSectionSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTeacherSectionStudentSectionSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTeacherSectionStudentSectionSetting <- function(TeacherSectionStudentSectionSettingID, TeacherSectionSettingID = F, StudentSectionID = F, DisplayOrder = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TeacherSectionStudentSectionSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TeacherSectionStudentSectionSetting", objectId = TeacherSectionStudentSectionSettingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GradebookTempAssignmentErrors
	#'
	#' This function returns a dataframe or json object of GradebookTempAssignmentErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookTempAssignmentError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of GradebookTempAssignmentErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGradebookTempAssignmentErrors <- function(searchConditionsList = NULL, TempAssignmentErrorID = F, ErrorText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempSectionAssignmentID = F, ErrorNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempAssignmentError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get GradebookTempAssignmentError
	#'
	#' This function returns a dataframe or json object of a GradebookTempAssignmentError
	#' @param GradebookTempAssignmentErrorID The ID of the GradebookTempAssignmentError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookTempAssignmentError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getGradebookTempAssignmentError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGradebookTempAssignmentError <- function(GradebookTempAssignmentErrorID, TempAssignmentErrorID = F, ErrorText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempSectionAssignmentID = F, ErrorNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GradebookTempAssignmentErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempAssignmentError", objectId = GradebookTempAssignmentErrorID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TeacherSectionStandardsDisplayGradeBucketSettings
	#'
	#' This function returns a dataframe or json object of TeacherSectionStandardsDisplayGradeBucketSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionStandardsDisplayGradeBucketSetting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TeacherSectionStandardsDisplayGradeBucketSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTeacherSectionStandardsDisplayGradeBucketSettings <- function(searchConditionsList = NULL, TeacherSectionStandardsDisplayGradeBucketSettingID = F, TeacherSectionSettingID = F, CalculationGroupGradeBucketID = F, ShowBucket = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionStandardsDisplayGradeBucketSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TeacherSectionStandardsDisplayGradeBucketSetting
	#'
	#' This function returns a dataframe or json object of a TeacherSectionStandardsDisplayGradeBucketSetting
	#' @param TeacherSectionStandardsDisplayGradeBucketSettingID The ID of the TeacherSectionStandardsDisplayGradeBucketSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionStandardsDisplayGradeBucketSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTeacherSectionStandardsDisplayGradeBucketSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTeacherSectionStandardsDisplayGradeBucketSetting <- function(TeacherSectionStandardsDisplayGradeBucketSettingID, TeacherSectionSettingID = F, CalculationGroupGradeBucketID = F, ShowBucket = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TeacherSectionStandardsDisplayGradeBucketSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TeacherSectionStandardsDisplayGradeBucketSetting", objectId = TeacherSectionStandardsDisplayGradeBucketSettingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAdjustedCategories
	#'
	#' This function returns a dataframe or json object of TempAdjustedCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempAdjustedCategory') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempAdjustedCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAdjustedCategories <- function(searchConditionsList = NULL, TempAdjustedCategoryID = F, TempSectionCategoryID = F, CategoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempAdjustedCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempAdjustedCategory
	#'
	#' This function returns a dataframe or json object of a TempAdjustedCategory
	#' @param TempAdjustedCategoryID The ID of the TempAdjustedCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempAdjustedCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempAdjustedCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAdjustedCategory <- function(TempAdjustedCategoryID, TempSectionCategoryID = F, CategoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAdjustedCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempAdjustedCategory", objectId = TempAdjustedCategoryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentAssignments
	#'
	#' This function returns a dataframe or json object of TempStudentAssignments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentAssignment') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempStudentAssignments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentAssignments <- function(searchConditionsList = NULL, TempStudentAssignmentID = F, StudentSectionID = F, AssignmentID = F, SectionID = F, Score = F, MaxScore = F, Weight = F, AssignmentName = F, AssignmentDescription = F, DueDate = F, Include = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsMissing = F, NoCount = F, ScoreClarifierID = F, ScoreClarifierCode = F, DueDateIsInGivenDateRange = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempStudentAssignment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempStudentAssignment
	#'
	#' This function returns a dataframe or json object of a TempStudentAssignment
	#' @param TempStudentAssignmentID The ID of the TempStudentAssignment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentAssignment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempStudentAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentAssignment <- function(TempStudentAssignmentID, StudentSectionID = F, AssignmentID = F, SectionID = F, Score = F, MaxScore = F, Weight = F, AssignmentName = F, AssignmentDescription = F, DueDate = F, Include = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsMissing = F, NoCount = F, ScoreClarifierID = F, ScoreClarifierCode = F, DueDateIsInGivenDateRange = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentAssignmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempStudentAssignment", objectId = TempStudentAssignmentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempGradingPeriodGradeBucketErrors
	#'
	#' This function returns a dataframe or json object of TempGradingPeriodGradeBucketErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempGradingPeriodGradeBucketError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempGradingPeriodGradeBucketErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempGradingPeriodGradeBucketErrors <- function(searchConditionsList = NULL, TempGradingPeriodGradeBucketErrorID = F, ErrorText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubjectOrStandardDescription = F, SubjectOrStandardKey = F, CalculationType = F, GradingPeriodGradeBucketID = F, ErrorNumber = F, GradeBucketLabelWithDates = F, StartDate = F, EndDate = F, StatusDisplayWithExtensionDate = F, SectionLengthCodeDescription = F, CalculationGroupID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempGradingPeriodGradeBucketError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempGradingPeriodGradeBucketError
	#'
	#' This function returns a dataframe or json object of a TempGradingPeriodGradeBucketError
	#' @param TempGradingPeriodGradeBucketErrorID The ID of the TempGradingPeriodGradeBucketError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempGradingPeriodGradeBucketError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempGradingPeriodGradeBucketError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempGradingPeriodGradeBucketError <- function(TempGradingPeriodGradeBucketErrorID, ErrorText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubjectOrStandardDescription = F, SubjectOrStandardKey = F, CalculationType = F, GradingPeriodGradeBucketID = F, ErrorNumber = F, GradeBucketLabelWithDates = F, StartDate = F, EndDate = F, StatusDisplayWithExtensionDate = F, SectionLengthCodeDescription = F, CalculationGroupID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempGradingPeriodGradeBucketErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempGradingPeriodGradeBucketError", objectId = TempGradingPeriodGradeBucketErrorID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TeacherSectionStandardsDisplayAcademicStandardSettings
	#'
	#' This function returns a dataframe or json object of TeacherSectionStandardsDisplayAcademicStandardSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionStandardsDisplayAcademicStandardSetting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TeacherSectionStandardsDisplayAcademicStandardSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTeacherSectionStandardsDisplayAcademicStandardSettings <- function(searchConditionsList = NULL, TeacherSectionStandardsDisplayAcademicStandardSettingID = F, AcademicStandardID = F, TeacherSectionStandardsDisplayGradeBucketSettingID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionStandardsDisplayAcademicStandardSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TeacherSectionStandardsDisplayAcademicStandardSetting
	#'
	#' This function returns a dataframe or json object of a TeacherSectionStandardsDisplayAcademicStandardSetting
	#' @param TeacherSectionStandardsDisplayAcademicStandardSettingID The ID of the TeacherSectionStandardsDisplayAcademicStandardSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionStandardsDisplayAcademicStandardSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTeacherSectionStandardsDisplayAcademicStandardSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTeacherSectionStandardsDisplayAcademicStandardSetting <- function(TeacherSectionStandardsDisplayAcademicStandardSettingID, AcademicStandardID = F, TeacherSectionStandardsDisplayGradeBucketSettingID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TeacherSectionStandardsDisplayAcademicStandardSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TeacherSectionStandardsDisplayAcademicStandardSetting", objectId = TeacherSectionStandardsDisplayAcademicStandardSettingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentGradeBucketAcademicStandards
	#'
	#' This function returns a dataframe or json object of StudentGradeBucketAcademicStandards
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGradeBucketAcademicStandard') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StudentGradeBucketAcademicStandards
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentGradeBucketAcademicStandards <- function(searchConditionsList = NULL, StudentGradeBucketAcademicStandardID = F, StudentGradeBucketID = F, AcademicStandardID = F, GradeMarkID = F, Percent = F, EarnedPoints = F, PossiblePoints = F, SectionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAdminOverride = F, GradeMarkIDOverride = F, GradeMarkOverrideComment = F, PercentAdjustment = F, PercentAdjustmentComment = F, PercentWithAdjustment = F, PercentWithAdjustmentWithGradeMarkToUse = F, PercentWithGradeMark = F, GradeMarkToUse = F, CalculatedPoints = F, IsUsingPointsBasedScale = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGradeBucketAcademicStandard", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentGradeBucketAcademicStandard
	#'
	#' This function returns a dataframe or json object of a StudentGradeBucketAcademicStandard
	#' @param StudentGradeBucketAcademicStandardID The ID of the StudentGradeBucketAcademicStandard to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGradeBucketAcademicStandard') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStudentGradeBucketAcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentGradeBucketAcademicStandard <- function(StudentGradeBucketAcademicStandardID, StudentGradeBucketID = F, AcademicStandardID = F, GradeMarkID = F, Percent = F, EarnedPoints = F, PossiblePoints = F, SectionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAdminOverride = F, GradeMarkIDOverride = F, GradeMarkOverrideComment = F, PercentAdjustment = F, PercentAdjustmentComment = F, PercentWithAdjustment = F, PercentWithAdjustmentWithGradeMarkToUse = F, PercentWithGradeMark = F, GradeMarkToUse = F, CalculatedPoints = F, IsUsingPointsBasedScale = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentGradeBucketAcademicStandardID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StudentGradeBucketAcademicStandard", objectId = StudentGradeBucketAcademicStandardID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCourseMoveCourses
	#'
	#' This function returns a dataframe or json object of TempCourseMoveCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourseMoveCourse') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempCourseMoveCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCourseMoveCourses <- function(searchConditionsList = NULL, TempCourseMoveCourseID = F, CourseID = F, CourseCode = F, CourseDescription = F, OriginalCalculationGroupID = F, OriginalCalculationGroupDescription = F, NewCalculationGroupID = F, NewCalculationGroupDescription = F, IsValidUpdate = F, EntityID = F, SchoolYearID = F, GroupCourseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CodeDescription = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCourseMoveCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempCourseMoveCourse
	#'
	#' This function returns a dataframe or json object of a TempCourseMoveCourse
	#' @param TempCourseMoveCourseID The ID of the TempCourseMoveCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourseMoveCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempCourseMoveCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCourseMoveCourse <- function(TempCourseMoveCourseID, CourseID = F, CourseCode = F, CourseDescription = F, OriginalCalculationGroupID = F, OriginalCalculationGroupDescription = F, NewCalculationGroupID = F, NewCalculationGroupDescription = F, IsValidUpdate = F, EntityID = F, SchoolYearID = F, GroupCourseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CodeDescription = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCourseMoveCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempCourseMoveCourse", objectId = TempCourseMoveCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCourseMoveErrors
	#'
	#' This function returns a dataframe or json object of TempCourseMoveErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourseMoveError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempCourseMoveErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCourseMoveErrors <- function(searchConditionsList = NULL, TempCourseMoveErrorID = F, SectionID = F, BucketLabel = F, ErrorCode = F, ErrorMessage = F, ErrorType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCourseMoveError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempCourseMoveError
	#'
	#' This function returns a dataframe or json object of a TempCourseMoveError
	#' @param TempCourseMoveErrorID The ID of the TempCourseMoveError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourseMoveError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempCourseMoveError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCourseMoveError <- function(TempCourseMoveErrorID, SectionID = F, BucketLabel = F, ErrorCode = F, ErrorMessage = F, ErrorType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCourseMoveErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempCourseMoveError", objectId = TempCourseMoveErrorID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCourseMoveSectionErrors
	#'
	#' This function returns a dataframe or json object of TempCourseMoveSectionErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourseMoveSectionError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempCourseMoveSectionErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCourseMoveSectionErrors <- function(searchConditionsList = NULL, TempCourseMoveSectionErrorID = F, CourseID = F, SectionID = F, SectionCode = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCourseMoveSectionError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempCourseMoveSectionError
	#'
	#' This function returns a dataframe or json object of a TempCourseMoveSectionError
	#' @param TempCourseMoveSectionErrorID The ID of the TempCourseMoveSectionError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourseMoveSectionError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempCourseMoveSectionError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCourseMoveSectionError <- function(TempCourseMoveSectionErrorID, CourseID = F, SectionID = F, SectionCode = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCourseMoveSectionErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempCourseMoveSectionError", objectId = TempCourseMoveSectionErrorID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempGradeMarkErrors
	#'
	#' This function returns a dataframe or json object of TempGradeMarkErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempGradeMarkError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempGradeMarkErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempGradeMarkErrors <- function(searchConditionsList = NULL, TempGradeMarkErrorID = F, GradeMarkID = F, GradeMarkCode = F, SectionID = F, DisplayOrder = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempGradeMarkError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempGradeMarkError
	#'
	#' This function returns a dataframe or json object of a TempGradeMarkError
	#' @param TempGradeMarkErrorID The ID of the TempGradeMarkError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempGradeMarkError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempGradeMarkError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempGradeMarkError <- function(TempGradeMarkErrorID, GradeMarkID = F, GradeMarkCode = F, SectionID = F, DisplayOrder = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempGradeMarkErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempGradeMarkError", objectId = TempGradeMarkErrorID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSectionGradingScaleGroupToCreates
	#'
	#' This function returns a dataframe or json object of TempSectionGradingScaleGroupToCreates
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionGradingScaleGroupToCreate') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempSectionGradingScaleGroupToCreates
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSectionGradingScaleGroupToCreates <- function(searchConditionsList = NULL, TempSectionGradingScaleGroupToCreateID = F, CourseGradingScaleGroupID = F, CourseGradingScaleGroupCourseID = F, NewCourseGradingScaleGroupID = F, NewCourseGradingScaleGroupDescription = F, SectionID = F, CourseID = F, SectionCode = F, Description = F, RequiresSectionGradingScaleGroups = F, Reason = F, GradingPeriodGradeBucketsToRetainJson = F, IsValidInNewGroup = F, SectionLengthCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseCode = F, InvalidGradeMarks = F, CourseDescriptionCodeSectionCode = F, StaffFullNameLFM = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempSectionGradingScaleGroupToCreate", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempSectionGradingScaleGroupToCreate
	#'
	#' This function returns a dataframe or json object of a TempSectionGradingScaleGroupToCreate
	#' @param TempSectionGradingScaleGroupToCreateID The ID of the TempSectionGradingScaleGroupToCreate to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionGradingScaleGroupToCreate') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempSectionGradingScaleGroupToCreate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSectionGradingScaleGroupToCreate <- function(TempSectionGradingScaleGroupToCreateID, CourseGradingScaleGroupID = F, CourseGradingScaleGroupCourseID = F, NewCourseGradingScaleGroupID = F, NewCourseGradingScaleGroupDescription = F, SectionID = F, CourseID = F, SectionCode = F, Description = F, RequiresSectionGradingScaleGroups = F, Reason = F, GradingPeriodGradeBucketsToRetainJson = F, IsValidInNewGroup = F, SectionLengthCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseCode = F, InvalidGradeMarks = F, CourseDescriptionCodeSectionCode = F, StaffFullNameLFM = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSectionGradingScaleGroupToCreateID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempSectionGradingScaleGroupToCreate", objectId = TempSectionGradingScaleGroupToCreateID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TeacherSectionCategoryAnalyticsSettings
	#'
	#' This function returns a dataframe or json object of TeacherSectionCategoryAnalyticsSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionCategoryAnalyticsSetting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TeacherSectionCategoryAnalyticsSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTeacherSectionCategoryAnalyticsSettings <- function(searchConditionsList = NULL, TeacherSectionCategoryAnalyticsSettingID = F, CategoryID = F, TeacherSectionSettingID = F, AnalysisType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionCategoryAnalyticsSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TeacherSectionCategoryAnalyticsSetting
	#'
	#' This function returns a dataframe or json object of a TeacherSectionCategoryAnalyticsSetting
	#' @param TeacherSectionCategoryAnalyticsSettingID The ID of the TeacherSectionCategoryAnalyticsSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionCategoryAnalyticsSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTeacherSectionCategoryAnalyticsSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTeacherSectionCategoryAnalyticsSetting <- function(TeacherSectionCategoryAnalyticsSettingID, CategoryID = F, TeacherSectionSettingID = F, AnalysisType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TeacherSectionCategoryAnalyticsSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TeacherSectionCategoryAnalyticsSetting", objectId = TeacherSectionCategoryAnalyticsSettingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Assessments
	#'
	#' This function returns a dataframe or json object of Assessments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Assessment') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of Assessments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssessments <- function(searchConditionsList = NULL, AssessmentID = F, SectionID = F, CategoryID = F, AssignedDate = F, DueDate = F, Name = F, Description = F, IsDeleted = F, CanDelete = F, DisplayRestoreButton = F, AssignmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "Assessment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get Assessment
	#'
	#' This function returns a dataframe or json object of an Assessment
	#' @param AssessmentID The ID of the Assessment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Assessment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getAssessment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssessment <- function(AssessmentID, SectionID = F, CategoryID = F, AssignedDate = F, DueDate = F, Name = F, Description = F, IsDeleted = F, CanDelete = F, DisplayRestoreButton = F, AssignmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssessmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "Assessment", objectId = AssessmentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CategoryGradeBucketTypes
	#'
	#' This function returns a dataframe or json object of CategoryGradeBucketTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CategoryGradeBucketType') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CategoryGradeBucketTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCategoryGradeBucketTypes <- function(searchConditionsList = NULL, CategoryGradeBucketTypeID = F, CategoryID = F, GradeBucketTypeID = F, EntityGroupKey = F, CategoryGradeBucketTypeIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CategoryGradeBucketType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CategoryGradeBucketType
	#'
	#' This function returns a dataframe or json object of a CategoryGradeBucketType
	#' @param CategoryGradeBucketTypeID The ID of the CategoryGradeBucketType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CategoryGradeBucketType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCategoryGradeBucketType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCategoryGradeBucketType <- function(CategoryGradeBucketTypeID, CategoryID = F, GradeBucketTypeID = F, EntityGroupKey = F, CategoryGradeBucketTypeIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CategoryGradeBucketTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CategoryGradeBucketType", objectId = CategoryGradeBucketTypeID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GradebookConfigEntityGroupYears
	#'
	#' This function returns a dataframe or json object of GradebookConfigEntityGroupYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookConfigEntityGroupYear') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of GradebookConfigEntityGroupYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGradebookConfigEntityGroupYears <- function(searchConditionsList = NULL, ConfigEntityGroupYearID = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, GradingPeriodEditExtensionDays = F, GradingPeriodEditExtensionEndTime = F, CapWeightedCategoryCalculationAt100Percent = F, AllowRetainedFutureScoring = F, OnlyShowGradebooksWithActiveStudentSectionTransactions = F, ClosedGradingPeriodGradeChangePermissionType = F, ClosedGradingPeriodGradeChangeCutOff = F, AllowPercentAdjustment = F, NumberOfDaysUntilUnscoredAssignmentsAreMissing = F, NumberOfDaysUntilNewStudentIconAppears = F, AssignmentMaxScore = F, AllowTeachersToOverrideDefaultMaxScore = F, AssignmentMaxWeight = F, AllowTeachersToOverrideDefaultMaxWeight = F, AllowTeacherComments = F, CapCalculationAt100Percent = F, ConfigEntityGroupYearIDClonedFrom = F, DisplayNonGradedClassesForTeachers = F, AllowOnlineAssignments = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AutoApproveGradeChangeRequest = F, AllowTeachersToTransferGrades = F, AllowTeachersToAddStudentNotes = F, UseFailingGradeScoreClarifier = F, FailingGradeThresholdPercent = F, ScoreClarifierIDFailing = F, AllowNegativePercentAdjustment = F, AllowGracePeriodAtEndOfGradingPeriod = F, AllowMultipleAssignmentAttempts = F, AllowStudentGroups = F, AllowClosedGradingPeriodGradeChangeRequestsToUpdateSnapshotGrades = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ConfigEntityGroupYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get GradebookConfigEntityGroupYear
	#'
	#' This function returns a dataframe or json object of a GradebookConfigEntityGroupYear
	#' @param GradebookConfigEntityGroupYearID The ID of the GradebookConfigEntityGroupYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookConfigEntityGroupYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getGradebookConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGradebookConfigEntityGroupYear <- function(GradebookConfigEntityGroupYearID, ConfigEntityGroupYearID = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, GradingPeriodEditExtensionDays = F, GradingPeriodEditExtensionEndTime = F, CapWeightedCategoryCalculationAt100Percent = F, AllowRetainedFutureScoring = F, OnlyShowGradebooksWithActiveStudentSectionTransactions = F, ClosedGradingPeriodGradeChangePermissionType = F, ClosedGradingPeriodGradeChangeCutOff = F, AllowPercentAdjustment = F, NumberOfDaysUntilUnscoredAssignmentsAreMissing = F, NumberOfDaysUntilNewStudentIconAppears = F, AssignmentMaxScore = F, AllowTeachersToOverrideDefaultMaxScore = F, AssignmentMaxWeight = F, AllowTeachersToOverrideDefaultMaxWeight = F, AllowTeacherComments = F, CapCalculationAt100Percent = F, ConfigEntityGroupYearIDClonedFrom = F, DisplayNonGradedClassesForTeachers = F, AllowOnlineAssignments = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AutoApproveGradeChangeRequest = F, AllowTeachersToTransferGrades = F, AllowTeachersToAddStudentNotes = F, UseFailingGradeScoreClarifier = F, FailingGradeThresholdPercent = F, ScoreClarifierIDFailing = F, AllowNegativePercentAdjustment = F, AllowGracePeriodAtEndOfGradingPeriod = F, AllowMultipleAssignmentAttempts = F, AllowStudentGroups = F, AllowClosedGradingPeriodGradeChangeRequestsToUpdateSnapshotGrades = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GradebookConfigEntityGroupYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "ConfigEntityGroupYear", objectId = GradebookConfigEntityGroupYearID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ClosedGradingPeriodStudentGradeBucketChanges
	#'
	#' This function returns a dataframe or json object of ClosedGradingPeriodStudentGradeBucketChanges
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ClosedGradingPeriodStudentGradeBucketChange') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of ClosedGradingPeriodStudentGradeBucketChanges
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listClosedGradingPeriodStudentGradeBucketChanges <- function(searchConditionsList = NULL, ClosedGradingPeriodStudentGradeBucketChangeID = F, ClosedGradingPeriodGradeChangeID = F, StaffMeetID = F, StudentGradeBucketID = F, GradeMarkIDOriginal = F, GradeMarkIDNew = F, ClosedGradingPeriodGradeChangeStatus = F, IsDisabled = F, IsGradeChangePastDueAndInProgress = F, CalculatedClosedGradingPeriodGradeChangeStatus = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OriginalPercent = F, NewPercent = F, HasSnapshot = F, ChangeMadeByFullnameLFM = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ClosedGradingPeriodStudentGradeBucketChange", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get ClosedGradingPeriodStudentGradeBucketChange
	#'
	#' This function returns a dataframe or json object of a ClosedGradingPeriodStudentGradeBucketChange
	#' @param ClosedGradingPeriodStudentGradeBucketChangeID The ID of the ClosedGradingPeriodStudentGradeBucketChange to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ClosedGradingPeriodStudentGradeBucketChange') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getClosedGradingPeriodStudentGradeBucketChange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getClosedGradingPeriodStudentGradeBucketChange <- function(ClosedGradingPeriodStudentGradeBucketChangeID, ClosedGradingPeriodGradeChangeID = F, StaffMeetID = F, StudentGradeBucketID = F, GradeMarkIDOriginal = F, GradeMarkIDNew = F, ClosedGradingPeriodGradeChangeStatus = F, IsDisabled = F, IsGradeChangePastDueAndInProgress = F, CalculatedClosedGradingPeriodGradeChangeStatus = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OriginalPercent = F, NewPercent = F, HasSnapshot = F, ChangeMadeByFullnameLFM = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ClosedGradingPeriodStudentGradeBucketChangeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "ClosedGradingPeriodStudentGradeBucketChange", objectId = ClosedGradingPeriodStudentGradeBucketChangeID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ClosedGradingPeriodGradeChanges
	#'
	#' This function returns a dataframe or json object of ClosedGradingPeriodGradeChanges
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ClosedGradingPeriodGradeChange') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of ClosedGradingPeriodGradeChanges
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listClosedGradingPeriodGradeChanges <- function(searchConditionsList = NULL, ClosedGradingPeriodGradeChangeID = F, SectionID = F, GradingPeriodID = F, CompletedTime = F, Reason = F, DisplayCompleteButton = F, CalculatedCompletedTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayMassReviewDenyButton = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ClosedGradingPeriodGradeChange", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get ClosedGradingPeriodGradeChange
	#'
	#' This function returns a dataframe or json object of a ClosedGradingPeriodGradeChange
	#' @param ClosedGradingPeriodGradeChangeID The ID of the ClosedGradingPeriodGradeChange to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ClosedGradingPeriodGradeChange') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getClosedGradingPeriodGradeChange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getClosedGradingPeriodGradeChange <- function(ClosedGradingPeriodGradeChangeID, SectionID = F, GradingPeriodID = F, CompletedTime = F, Reason = F, DisplayCompleteButton = F, CalculatedCompletedTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayMassReviewDenyButton = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ClosedGradingPeriodGradeChangeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "ClosedGradingPeriodGradeChange", objectId = ClosedGradingPeriodGradeChangeID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TeacherSectionGradeBucketAnalyticsSettings
	#'
	#' This function returns a dataframe or json object of TeacherSectionGradeBucketAnalyticsSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionGradeBucketAnalyticsSetting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TeacherSectionGradeBucketAnalyticsSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTeacherSectionGradeBucketAnalyticsSettings <- function(searchConditionsList = NULL, TeacherSectionGradeBucketAnalyticsSettingID = F, CalculationGroupGradeBucketID = F, TeacherSectionSettingID = F, AnalysisType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionGradeBucketAnalyticsSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TeacherSectionGradeBucketAnalyticsSetting
	#'
	#' This function returns a dataframe or json object of a TeacherSectionGradeBucketAnalyticsSetting
	#' @param TeacherSectionGradeBucketAnalyticsSettingID The ID of the TeacherSectionGradeBucketAnalyticsSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionGradeBucketAnalyticsSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTeacherSectionGradeBucketAnalyticsSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTeacherSectionGradeBucketAnalyticsSetting <- function(TeacherSectionGradeBucketAnalyticsSettingID, CalculationGroupGradeBucketID = F, TeacherSectionSettingID = F, AnalysisType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TeacherSectionGradeBucketAnalyticsSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TeacherSectionGradeBucketAnalyticsSetting", objectId = TeacherSectionGradeBucketAnalyticsSettingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentSectionNotes
	#'
	#' This function returns a dataframe or json object of StudentSectionNotes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentSectionNote') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StudentSectionNotes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentSectionNotes <- function(searchConditionsList = NULL, StudentSectionNoteID = F, StudentSectionID = F, Note = F, OtherTeachersHaveAccess = F, LimitToThisSection = F, CurrentUserCanModify = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentSectionNote", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentSectionNote
	#'
	#' This function returns a dataframe or json object of a StudentSectionNote
	#' @param StudentSectionNoteID The ID of the StudentSectionNote to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentSectionNote') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStudentSectionNote
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentSectionNote <- function(StudentSectionNoteID, StudentSectionID = F, Note = F, OtherTeachersHaveAccess = F, LimitToThisSection = F, CurrentUserCanModify = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentSectionNoteID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StudentSectionNote", objectId = StudentSectionNoteID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionGradeBucketSettings
	#'
	#' This function returns a dataframe or json object of SectionGradeBucketSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionGradeBucketSetting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of SectionGradeBucketSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionGradeBucketSettings <- function(searchConditionsList = NULL, SectionGradeBucketSettingID = F, SectionID = F, GradingPeriodGradeBucketID = F, UseMaxExtraCredit = F, MaxExtraCredit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionGradeBucketSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionGradeBucketSetting
	#'
	#' This function returns a dataframe or json object of a SectionGradeBucketSetting
	#' @param SectionGradeBucketSettingID The ID of the SectionGradeBucketSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionGradeBucketSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getSectionGradeBucketSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionGradeBucketSetting <- function(SectionGradeBucketSettingID, SectionID = F, GradingPeriodGradeBucketID = F, UseMaxExtraCredit = F, MaxExtraCredit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionGradeBucketSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "SectionGradeBucketSetting", objectId = SectionGradeBucketSettingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionGradingPeriodData
	#'
	#' This function returns a dataframe or json object of SectionGradingPeriodData
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionGradingPeriodData') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of SectionGradingPeriodData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionGradingPeriodData <- function(searchConditionsList = NULL, SectionGradingPeriodDataID = F, SectionID = F, GradingPeriodID = F, IsCompleted = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionGradingPeriodData", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionGradingPeriodData
	#'
	#' This function returns a dataframe or json object of a SectionGradingPeriodData
	#' @param SectionGradingPeriodDataID The ID of the SectionGradingPeriodData to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionGradingPeriodData') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getSectionGradingPeriodData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionGradingPeriodData <- function(SectionGradingPeriodDataID, SectionID = F, GradingPeriodID = F, IsCompleted = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionGradingPeriodDataID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "SectionGradingPeriodData", objectId = SectionGradingPeriodDataID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TeacherSectionGradingPeriodSettings
	#'
	#' This function returns a dataframe or json object of TeacherSectionGradingPeriodSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionGradingPeriodSetting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TeacherSectionGradingPeriodSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTeacherSectionGradingPeriodSettings <- function(searchConditionsList = NULL, TeacherSectionGradingPeriodSettingID = F, TeacherSectionSettingID = F, GradingPeriodID = F, ShowGradeBuckets = F, ShowAssignments = F, ShowAssessments = F, IncludeMissingAssignments = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionGradingPeriodSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TeacherSectionGradingPeriodSetting
	#'
	#' This function returns a dataframe or json object of a TeacherSectionGradingPeriodSetting
	#' @param TeacherSectionGradingPeriodSettingID The ID of the TeacherSectionGradingPeriodSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionGradingPeriodSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTeacherSectionGradingPeriodSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTeacherSectionGradingPeriodSetting <- function(TeacherSectionGradingPeriodSettingID, TeacherSectionSettingID = F, GradingPeriodID = F, ShowGradeBuckets = F, ShowAssignments = F, ShowAssessments = F, IncludeMissingAssignments = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TeacherSectionGradingPeriodSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TeacherSectionGradingPeriodSetting", objectId = TeacherSectionGradingPeriodSettingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TeacherSectionGradeBucketSettings
	#'
	#' This function returns a dataframe or json object of TeacherSectionGradeBucketSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionGradeBucketSetting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TeacherSectionGradeBucketSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTeacherSectionGradeBucketSettings <- function(searchConditionsList = NULL, TeacherSectionGradeBucketSettingID = F, TeacherSectionSettingID = F, GradingPeriodGradeBucketID = F, GradeBucketDisplayType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsExpanded = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionGradeBucketSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TeacherSectionGradeBucketSetting
	#'
	#' This function returns a dataframe or json object of a TeacherSectionGradeBucketSetting
	#' @param TeacherSectionGradeBucketSettingID The ID of the TeacherSectionGradeBucketSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionGradeBucketSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTeacherSectionGradeBucketSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTeacherSectionGradeBucketSetting <- function(TeacherSectionGradeBucketSettingID, TeacherSectionSettingID = F, GradingPeriodGradeBucketID = F, GradeBucketDisplayType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsExpanded = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TeacherSectionGradeBucketSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TeacherSectionGradeBucketSetting", objectId = TeacherSectionGradeBucketSettingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TeacherSectionSettings
	#'
	#' This function returns a dataframe or json object of TeacherSectionSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionSetting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TeacherSectionSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTeacherSectionSettings <- function(searchConditionsList = NULL, TeacherSectionSettingID = F, SectionID = F, ClassGroupID = F, UserIDOwner = F, ManualDateEntryStartDate = F, ManualDateEntryEndDate = F, StudentsToDisplay = F, StudentNameDisplayType = F, DisplayStudentGradeLevel = F, DisplayStudentNumber = F, DisplayAttendance = F, DisplayMissingAssignment = F, DisplayUnscoredPastDueAssignments = F, DisplayGradebookAverages = F, DisplayAssignmentAttendanceIndicator = F, DisplayStudentGradingPeriodCommentIndicator = F, CalculationGroupGradeBucketIDLocked = F, BrowseViewID = F, HideLockedColumns = F, ShowGradebookLockedColumnButton = F, UseCustomClassRosterSort = F, HasCustomClassRosterSort = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HideAssignmentCategoryColors = F, DisplayStudentGroups = F, DisplayStudentGroupColors = F, DisplayAssignmentStudentGroupColors = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TeacherSectionSetting
	#'
	#' This function returns a dataframe or json object of a TeacherSectionSetting
	#' @param TeacherSectionSettingID The ID of the TeacherSectionSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTeacherSectionSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTeacherSectionSetting <- function(TeacherSectionSettingID, SectionID = F, ClassGroupID = F, UserIDOwner = F, ManualDateEntryStartDate = F, ManualDateEntryEndDate = F, StudentsToDisplay = F, StudentNameDisplayType = F, DisplayStudentGradeLevel = F, DisplayStudentNumber = F, DisplayAttendance = F, DisplayMissingAssignment = F, DisplayUnscoredPastDueAssignments = F, DisplayGradebookAverages = F, DisplayAssignmentAttendanceIndicator = F, DisplayStudentGradingPeriodCommentIndicator = F, CalculationGroupGradeBucketIDLocked = F, BrowseViewID = F, HideLockedColumns = F, ShowGradebookLockedColumnButton = F, UseCustomClassRosterSort = F, HasCustomClassRosterSort = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HideAssignmentCategoryColors = F, DisplayStudentGroups = F, DisplayStudentGroupColors = F, DisplayAssignmentStudentGroupColors = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TeacherSectionSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TeacherSectionSetting", objectId = TeacherSectionSettingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GradesheetAssignmentSettings
	#'
	#' This function returns a dataframe or json object of GradesheetAssignmentSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradesheetAssignmentSetting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of GradesheetAssignmentSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGradesheetAssignmentSettings <- function(searchConditionsList = NULL, GradesheetAssignmentSettingID = F, TeacherSectionSettingID = F, MaxScoreDefault = F, IsDateAscending = F, SortBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DefaultAttemptType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "GradesheetAssignmentSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get GradesheetAssignmentSetting
	#'
	#' This function returns a dataframe or json object of a GradesheetAssignmentSetting
	#' @param GradesheetAssignmentSettingID The ID of the GradesheetAssignmentSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradesheetAssignmentSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getGradesheetAssignmentSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGradesheetAssignmentSetting <- function(GradesheetAssignmentSettingID, TeacherSectionSettingID = F, MaxScoreDefault = F, IsDateAscending = F, SortBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DefaultAttemptType = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GradesheetAssignmentSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "GradesheetAssignmentSetting", objectId = GradesheetAssignmentSettingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSectionGradingScales
	#'
	#' This function returns a dataframe or json object of TempSectionGradingScales
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionGradingScale') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempSectionGradingScales
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSectionGradingScales <- function(searchConditionsList = NULL, TempSectionGradingScaleID = F, SectionID = F, CanClone = F, ErrorMessage = F, ChildGradeBuckets = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempSectionGradingScale", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempSectionGradingScale
	#'
	#' This function returns a dataframe or json object of a TempSectionGradingScale
	#' @param TempSectionGradingScaleID The ID of the TempSectionGradingScale to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionGradingScale') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempSectionGradingScale
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSectionGradingScale <- function(TempSectionGradingScaleID, SectionID = F, CanClone = F, ErrorMessage = F, ChildGradeBuckets = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSectionGradingScaleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempSectionGradingScale", objectId = TempSectionGradingScaleID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCloneGradebookSections
	#'
	#' This function returns a dataframe or json object of TempCloneGradebookSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCloneGradebookSection') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempCloneGradebookSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCloneGradebookSections <- function(searchConditionsList = NULL, TempCloneGradebookSectionID = F, SectionID = F, CanClone = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCloneGradebookSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempCloneGradebookSection
	#'
	#' This function returns a dataframe or json object of a TempCloneGradebookSection
	#' @param TempCloneGradebookSectionID The ID of the TempCloneGradebookSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCloneGradebookSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempCloneGradebookSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCloneGradebookSection <- function(TempCloneGradebookSectionID, SectionID = F, CanClone = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCloneGradebookSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempCloneGradebookSection", objectId = TempCloneGradebookSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCalculationGroupWeightings
	#'
	#' This function returns a dataframe or json object of TempCalculationGroupWeightings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCalculationGroupWeighting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempCalculationGroupWeightings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCalculationGroupWeightings <- function(searchConditionsList = NULL, TempCalculationGroupWeightingID = F, CalculationGroupGradeBucketID = F, GradingPeriodGradeBucketID = F, CategoryID = F, CategoryDescription = F, GradeBucketLabel = F, WeightPercent = F, GradeBucketOrder = F, SubjectID = F, SubjectDescription = F, AcademicStandardID = F, AcademicStandardDescription = F, AcademicStandardFullKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCalculationGroupWeighting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempCalculationGroupWeighting
	#'
	#' This function returns a dataframe or json object of a TempCalculationGroupWeighting
	#' @param TempCalculationGroupWeightingID The ID of the TempCalculationGroupWeighting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCalculationGroupWeighting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempCalculationGroupWeighting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCalculationGroupWeighting <- function(TempCalculationGroupWeightingID, CalculationGroupGradeBucketID = F, GradingPeriodGradeBucketID = F, CategoryID = F, CategoryDescription = F, GradeBucketLabel = F, WeightPercent = F, GradeBucketOrder = F, SubjectID = F, SubjectDescription = F, AcademicStandardID = F, AcademicStandardDescription = F, AcademicStandardFullKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCalculationGroupWeightingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempCalculationGroupWeighting", objectId = TempCalculationGroupWeightingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempGradebookCloneErrors
	#'
	#' This function returns a dataframe or json object of TempGradebookCloneErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempGradebookCloneError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempGradebookCloneErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempGradebookCloneErrors <- function(searchConditionsList = NULL, TempGradebookCloneErrorID = F, CalculationGroupGradeBucketID = F, SectionID = F, Field = F, Message = F, GradeBucketLabel = F, RecordType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempGradebookCloneError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempGradebookCloneError
	#'
	#' This function returns a dataframe or json object of a TempGradebookCloneError
	#' @param TempGradebookCloneErrorID The ID of the TempGradebookCloneError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempGradebookCloneError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempGradebookCloneError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempGradebookCloneError <- function(TempGradebookCloneErrorID, CalculationGroupGradeBucketID = F, SectionID = F, Field = F, Message = F, GradeBucketLabel = F, RecordType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempGradebookCloneErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempGradebookCloneError", objectId = TempGradebookCloneErrorID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSectionGradeBucketWeights
	#'
	#' This function returns a dataframe or json object of TempSectionGradeBucketWeights
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionGradeBucketWeight') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempSectionGradeBucketWeights
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSectionGradeBucketWeights <- function(searchConditionsList = NULL, TempSectionGradeBucketWeightID = F, CalculationGroupGradeBucketIDFeeder = F, CalculationGroupGradeBucketIDComposite = F, SectionID = F, WeightPercent = F, CanClone = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AcademicStandardIDToWeight = F, SubjectIDToWeight = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempSectionGradeBucketWeight", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempSectionGradeBucketWeight
	#'
	#' This function returns a dataframe or json object of a TempSectionGradeBucketWeight
	#' @param TempSectionGradeBucketWeightID The ID of the TempSectionGradeBucketWeight to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionGradeBucketWeight') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempSectionGradeBucketWeight
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSectionGradeBucketWeight <- function(TempSectionGradeBucketWeightID, CalculationGroupGradeBucketIDFeeder = F, CalculationGroupGradeBucketIDComposite = F, SectionID = F, WeightPercent = F, CanClone = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AcademicStandardIDToWeight = F, SubjectIDToWeight = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSectionGradeBucketWeightID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempSectionGradeBucketWeight", objectId = TempSectionGradeBucketWeightID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSectionCategories
	#'
	#' This function returns a dataframe or json object of TempSectionCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionCategory') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempSectionCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSectionCategories <- function(searchConditionsList = NULL, TempSectionCategoryID = F, CalculationGroupGradeBucketID = F, CalculationGroupID = F, SectionID = F, CategoryID = F, CategoryCode = F, WeightPercent = F, CanClone = F, ErrorMessages = F, CategoryIDIsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempSectionCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempSectionCategory
	#'
	#' This function returns a dataframe or json object of a TempSectionCategory
	#' @param TempSectionCategoryID The ID of the TempSectionCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempSectionCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSectionCategory <- function(TempSectionCategoryID, CalculationGroupGradeBucketID = F, CalculationGroupID = F, SectionID = F, CategoryID = F, CategoryCode = F, WeightPercent = F, CanClone = F, ErrorMessages = F, CategoryIDIsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSectionCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempSectionCategory", objectId = TempSectionCategoryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionGradeBucketWeights
	#'
	#' This function returns a dataframe or json object of SectionGradeBucketWeights
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionGradeBucketWeight') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of SectionGradeBucketWeights
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionGradeBucketWeights <- function(searchConditionsList = NULL, SectionGradeBucketWeightID = F, SectionGradeBucketIDFeeder = F, SectionGradeBucketIDComposite = F, WeightPercent = F, RoundBucketPercent = F, AdjustedWeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionGradeBucketWeight", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionGradeBucketWeight
	#'
	#' This function returns a dataframe or json object of a SectionGradeBucketWeight
	#' @param SectionGradeBucketWeightID The ID of the SectionGradeBucketWeight to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionGradeBucketWeight') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getSectionGradeBucketWeight
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionGradeBucketWeight <- function(SectionGradeBucketWeightID, SectionGradeBucketIDFeeder = F, SectionGradeBucketIDComposite = F, WeightPercent = F, RoundBucketPercent = F, AdjustedWeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionGradeBucketWeightID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "SectionGradeBucketWeight", objectId = SectionGradeBucketWeightID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionCategories
	#'
	#' This function returns a dataframe or json object of SectionCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionCategory') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of SectionCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionCategories <- function(searchConditionsList = NULL, SectionCategoryID = F, SectionGradeBucketID = F, CategoryID = F, WeightPercent = F, PercentEarnedForCategory = F, HasAssignments = F, AdjustedWeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PointsEarnedForCategory = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionCategory
	#'
	#' This function returns a dataframe or json object of a SectionCategory
	#' @param SectionCategoryID The ID of the SectionCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getSectionCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionCategory <- function(SectionCategoryID, SectionGradeBucketID = F, CategoryID = F, WeightPercent = F, PercentEarnedForCategory = F, HasAssignments = F, AdjustedWeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PointsEarnedForCategory = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "SectionCategory", objectId = SectionCategoryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentSectionGradingScaleGradeBuckets
	#'
	#' This function returns a dataframe or json object of StudentSectionGradingScaleGradeBuckets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentSectionGradingScaleGradeBucket') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StudentSectionGradingScaleGradeBuckets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentSectionGradingScaleGradeBuckets <- function(searchConditionsList = NULL, StudentSectionGradingScaleGradeBucketID = F, GradingPeriodGradeBucketID = F, StudentGradingScaleGroupStudentSectionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentSectionGradingScaleGradeBucket", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentSectionGradingScaleGradeBucket
	#'
	#' This function returns a dataframe or json object of a StudentSectionGradingScaleGradeBucket
	#' @param StudentSectionGradingScaleGradeBucketID The ID of the StudentSectionGradingScaleGradeBucket to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentSectionGradingScaleGradeBucket') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStudentSectionGradingScaleGradeBucket
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentSectionGradingScaleGradeBucket <- function(StudentSectionGradingScaleGradeBucketID, GradingPeriodGradeBucketID = F, StudentGradingScaleGroupStudentSectionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentSectionGradingScaleGradeBucketID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StudentSectionGradingScaleGradeBucket", objectId = StudentSectionGradingScaleGradeBucketID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSectionGradeBuckets
	#'
	#' This function returns a dataframe or json object of TempSectionGradeBuckets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionGradeBucket') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempSectionGradeBuckets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSectionGradeBuckets <- function(searchConditionsList = NULL, TempSectionGradeBucketID = F, SectionGradeBucketID = F, CalculationGroupGradeBucketID = F, GradeBucketTypeID = F, IsSpecificGradeBucket = F, SectionID = F, GradeBucketLabel = F, CanClone = F, HasLeftOverPercentage = F, Exclude = F, ErrorMessages = F, StartDate = F, EndDate = F, ChildRecords = F, IsCalculationTypeOverridden = F, CurrentAllowCategoryOverride = F, CurrentCalculationType = F, NewCalculationType = F, Order = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempSectionGradeBucket", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempSectionGradeBucket
	#'
	#' This function returns a dataframe or json object of a TempSectionGradeBucket
	#' @param TempSectionGradeBucketID The ID of the TempSectionGradeBucket to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionGradeBucket') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempSectionGradeBucket
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSectionGradeBucket <- function(TempSectionGradeBucketID, SectionGradeBucketID = F, CalculationGroupGradeBucketID = F, GradeBucketTypeID = F, IsSpecificGradeBucket = F, SectionID = F, GradeBucketLabel = F, CanClone = F, HasLeftOverPercentage = F, Exclude = F, ErrorMessages = F, StartDate = F, EndDate = F, ChildRecords = F, IsCalculationTypeOverridden = F, CurrentAllowCategoryOverride = F, CurrentCalculationType = F, NewCalculationType = F, Order = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSectionGradeBucketID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempSectionGradeBucket", objectId = TempSectionGradeBucketID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDropLowScoreStudentAssignments
	#'
	#' This function returns a dataframe or json object of TempDropLowScoreStudentAssignments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempDropLowScoreStudentAssignment') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempDropLowScoreStudentAssignments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDropLowScoreStudentAssignments <- function(searchConditionsList = NULL, TempDropLowScoreStudentAssignmentID = F, StudentAssignmentID = F, StudentName = F, StudentSectionID = F, AssignmentName = F, DropCode = F, DueDate = F, MaxScore = F, Weight = F, DropLowScore = F, NoDropReason = F, Comments = F, Score = F, OriginalGrade = F, NewGrade = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentDisplaySortCode = F, DropActionType = F, UndoDropActionType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempDropLowScoreStudentAssignment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempDropLowScoreStudentAssignment
	#'
	#' This function returns a dataframe or json object of a TempDropLowScoreStudentAssignment
	#' @param TempDropLowScoreStudentAssignmentID The ID of the TempDropLowScoreStudentAssignment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempDropLowScoreStudentAssignment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempDropLowScoreStudentAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDropLowScoreStudentAssignment <- function(TempDropLowScoreStudentAssignmentID, StudentAssignmentID = F, StudentName = F, StudentSectionID = F, AssignmentName = F, DropCode = F, DueDate = F, MaxScore = F, Weight = F, DropLowScore = F, NoDropReason = F, Comments = F, Score = F, OriginalGrade = F, NewGrade = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentDisplaySortCode = F, DropActionType = F, UndoDropActionType = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDropLowScoreStudentAssignmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempDropLowScoreStudentAssignment", objectId = TempDropLowScoreStudentAssignmentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GradebookAssignments
	#'
	#' This function returns a dataframe or json object of GradebookAssignments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookAssignment') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of GradebookAssignments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGradebookAssignments <- function(searchConditionsList = NULL, AssignmentID = F, SectionID = F, CategoryID = F, AssignedDate = F, DueDate = F, Name = F, Description = F, MaxScore = F, Weight = F, ScoreDisplayType = F, AssignmentSyncKey = F, IsDeleted = F, IsOnlineAssignment = F, Instructions = F, StartTime = F, EndTime = F, DefaultPointsPerQuestion = F, RandomizeQuestions = F, AutoScore = F, ShowCorrectAnswers = F, AnswerRevealTime = F, AssessmentID = F, AcademicStandardID = F, SubjectID = F, CanDelete = F, DisplayRestoreButton = F, UseGradeMarkScoring = F, ScoredStudentAssignmentCount = F, ActiveStudentCount = F, HasUngradedStudentAssignments = F, HasQuestions = F, QuestionCount = F, AveragePercent = F, DueDateInCurrentOpenExtendedGradingPeriod = F, IsAHistoricRecord = F, DueDateIsInGivenDateRange = F, IsPastEndTime = F, AssignmentQuestionCount = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowAutoScore = F, CalculatedPointsAllowedFromQuestions = F, SendNotificationWhenAssignmentReadyToTake = F, SendNotificationWhenAnswersRevealed = F, HideScoreUntilTime = F, HasStudentAssignments = F, HasStudentAssignmentsWithScoreOrGradeMark = F, AllStudentAssignmentsAreNotStarted = F, HasStudentFamilyAttachmentsToDisplay = F, StudentFamilyAccessAttachmentCount = F, ShowScore = F, HasWholeNumberWeight = F, AssignmentIDParent = F, IsParent = F, HasParent = F, DueDateInTransactionForGivenStudentSection = F, RelatedAssignmentsHaveAcademicStandards = F, RelatedAssignmentsHaveSubjects = F, ChildAssignmentCount = F, Subjects = F, AcademicStandards = F, IsActiveOrUnlocked = F, HasStudentGroupAssignments = F, ActiveStudentGroups = F, ClassesSyncedTo = F, IsAvailableForStudentSection = F, IsAvailableForStudentGroup = F, CreateStudentGroupAssignmentErrorMessage = F, IsSynced = F, EffectiveAcademicStandardID = F, AssignmentIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "Assignment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get GradebookAssignment
	#'
	#' This function returns a dataframe or json object of a GradebookAssignment
	#' @param GradebookAssignmentID The ID of the GradebookAssignment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookAssignment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getGradebookAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGradebookAssignment <- function(GradebookAssignmentID, AssignmentID = F, SectionID = F, CategoryID = F, AssignedDate = F, DueDate = F, Name = F, Description = F, MaxScore = F, Weight = F, ScoreDisplayType = F, AssignmentSyncKey = F, IsDeleted = F, IsOnlineAssignment = F, Instructions = F, StartTime = F, EndTime = F, DefaultPointsPerQuestion = F, RandomizeQuestions = F, AutoScore = F, ShowCorrectAnswers = F, AnswerRevealTime = F, AssessmentID = F, AcademicStandardID = F, SubjectID = F, CanDelete = F, DisplayRestoreButton = F, UseGradeMarkScoring = F, ScoredStudentAssignmentCount = F, ActiveStudentCount = F, HasUngradedStudentAssignments = F, HasQuestions = F, QuestionCount = F, AveragePercent = F, DueDateInCurrentOpenExtendedGradingPeriod = F, IsAHistoricRecord = F, DueDateIsInGivenDateRange = F, IsPastEndTime = F, AssignmentQuestionCount = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowAutoScore = F, CalculatedPointsAllowedFromQuestions = F, SendNotificationWhenAssignmentReadyToTake = F, SendNotificationWhenAnswersRevealed = F, HideScoreUntilTime = F, HasStudentAssignments = F, HasStudentAssignmentsWithScoreOrGradeMark = F, AllStudentAssignmentsAreNotStarted = F, HasStudentFamilyAttachmentsToDisplay = F, StudentFamilyAccessAttachmentCount = F, ShowScore = F, HasWholeNumberWeight = F, AssignmentIDParent = F, IsParent = F, HasParent = F, DueDateInTransactionForGivenStudentSection = F, RelatedAssignmentsHaveAcademicStandards = F, RelatedAssignmentsHaveSubjects = F, ChildAssignmentCount = F, Subjects = F, AcademicStandards = F, IsActiveOrUnlocked = F, HasStudentGroupAssignments = F, ActiveStudentGroups = F, ClassesSyncedTo = F, IsAvailableForStudentSection = F, IsAvailableForStudentGroup = F, CreateStudentGroupAssignmentErrorMessage = F, IsSynced = F, EffectiveAcademicStandardID = F, AssignmentIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GradebookAssignmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "Assignment", objectId = GradebookAssignmentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GradebookCategories
	#'
	#' This function returns a dataframe or json object of GradebookCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookCategory') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of GradebookCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGradebookCategories <- function(searchConditionsList = NULL, CategoryID = F, Code = F, Description = F, DistrictID = F, SchoolYearID = F, DistrictGroupKey = F, CodeDescription = F, BackgroundColor = F, TextColor = F, UseForSpecificGradeBucketType = F, CategoryIDClonedFrom = F, AssignmentCount = F, GradeBucketTypeCategory = F, DescriptionWithCategoryWeightPercent = F, CategoryUsedInTotalPointsDefaultCalculationSetup = F, CategoryUsedInCategoryWeightingDefaultSetup = F, CalculationGroupAllowCategoryOverride = F, HasAssignments = F, AverageStudentAssignmentScoreForCategoryAndStudentSection = F, IsValidInCalculationGroup = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasSpecificCalculationGroupWeight = F, HasSpecificCalculationGroupSubjectWeight = F, HasSpecificCalculationGroupAcademicStandardWeight = F, WeightingAllowedForGradeBucketType = F, IsAHistoricRecord = F, EdFiGradebookEntryTypeDescriptorID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "Category", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get GradebookCategory
	#'
	#' This function returns a dataframe or json object of a GradebookCategory
	#' @param GradebookCategoryID The ID of the GradebookCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getGradebookCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGradebookCategory <- function(GradebookCategoryID, CategoryID = F, Code = F, Description = F, DistrictID = F, SchoolYearID = F, DistrictGroupKey = F, CodeDescription = F, BackgroundColor = F, TextColor = F, UseForSpecificGradeBucketType = F, CategoryIDClonedFrom = F, AssignmentCount = F, GradeBucketTypeCategory = F, DescriptionWithCategoryWeightPercent = F, CategoryUsedInTotalPointsDefaultCalculationSetup = F, CategoryUsedInCategoryWeightingDefaultSetup = F, CalculationGroupAllowCategoryOverride = F, HasAssignments = F, AverageStudentAssignmentScoreForCategoryAndStudentSection = F, IsValidInCalculationGroup = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasSpecificCalculationGroupWeight = F, HasSpecificCalculationGroupSubjectWeight = F, HasSpecificCalculationGroupAcademicStandardWeight = F, WeightingAllowedForGradeBucketType = F, IsAHistoricRecord = F, EdFiGradebookEntryTypeDescriptorID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GradebookCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "Category", objectId = GradebookCategoryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalculationGroupCategories
	#'
	#' This function returns a dataframe or json object of CalculationGroupCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupCategory') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CalculationGroupCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalculationGroupCategories <- function(searchConditionsList = NULL, CalculationGroupCategoryID = F, CalculationGroupID = F, CategoryID = F, EntityGroupKey = F, HasAssignments = F, CalculationGroupCategoryIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CalculationGroupCategory
	#'
	#' This function returns a dataframe or json object of a CalculationGroupCategory
	#' @param CalculationGroupCategoryID The ID of the CalculationGroupCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCalculationGroupCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalculationGroupCategory <- function(CalculationGroupCategoryID, CalculationGroupID = F, CategoryID = F, EntityGroupKey = F, HasAssignments = F, CalculationGroupCategoryIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalculationGroupCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CalculationGroupCategory", objectId = CalculationGroupCategoryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalculationGroupCourses
	#'
	#' This function returns a dataframe or json object of CalculationGroupCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupCourse') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CalculationGroupCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalculationGroupCourses <- function(searchConditionsList = NULL, CalculationGroupCourseID = F, CalculationGroupID = F, CourseID = F, CalculationGroupCourseIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CalculationGroupCourse
	#'
	#' This function returns a dataframe or json object of a CalculationGroupCourse
	#' @param CalculationGroupCourseID The ID of the CalculationGroupCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCalculationGroupCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalculationGroupCourse <- function(CalculationGroupCourseID, CalculationGroupID = F, CourseID = F, CalculationGroupCourseIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalculationGroupCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CalculationGroupCourse", objectId = CalculationGroupCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalculationGroupGradeBuckets
	#'
	#' This function returns a dataframe or json object of CalculationGroupGradeBuckets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupGradeBucket') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CalculationGroupGradeBuckets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalculationGroupGradeBuckets <- function(searchConditionsList = NULL, CalculationGroupGradeBucketID = F, CalculationGroupID = F, GradingPeriodGradeBucketID = F, AllowCalculationTypeOverride = F, AllowWeightOverride = F, CalculationType = F, CalculationGroupGradeBucketIDClonedFrom = F, RoundBucketPercent = F, GradeMarkScoringType = F, IsAHistoricRecord = F, HasCategoryWeighting = F, HasGradeBucketWeighting = F, HasSubjectWeighting = F, HasAcademicStandardWeighting = F, WeightPercentTotal = F, InnerCalculationGroupGradeBucketsCount = F, EffectiveCalculationTypeCode = F, NumberOfCategories = F, HasSubjects = F, HasAcademicStandards = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityGroupKey = F, CourseCount = F, InUseBySections = F, IsMissingStudentGradeBucketAcademicStandards = F, IsMissingStudentGradeBucketSubjects = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupGradeBucket", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CalculationGroupGradeBucket
	#'
	#' This function returns a dataframe or json object of a CalculationGroupGradeBucket
	#' @param CalculationGroupGradeBucketID The ID of the CalculationGroupGradeBucket to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupGradeBucket') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCalculationGroupGradeBucket
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalculationGroupGradeBucket <- function(CalculationGroupGradeBucketID, CalculationGroupID = F, GradingPeriodGradeBucketID = F, AllowCalculationTypeOverride = F, AllowWeightOverride = F, CalculationType = F, CalculationGroupGradeBucketIDClonedFrom = F, RoundBucketPercent = F, GradeMarkScoringType = F, IsAHistoricRecord = F, HasCategoryWeighting = F, HasGradeBucketWeighting = F, HasSubjectWeighting = F, HasAcademicStandardWeighting = F, WeightPercentTotal = F, InnerCalculationGroupGradeBucketsCount = F, EffectiveCalculationTypeCode = F, NumberOfCategories = F, HasSubjects = F, HasAcademicStandards = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityGroupKey = F, CourseCount = F, InUseBySections = F, IsMissingStudentGradeBucketAcademicStandards = F, IsMissingStudentGradeBucketSubjects = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalculationGroupGradeBucketID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CalculationGroupGradeBucket", objectId = CalculationGroupGradeBucketID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalculationGroupWeightings
	#'
	#' This function returns a dataframe or json object of CalculationGroupWeightings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupWeighting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CalculationGroupWeightings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalculationGroupWeightings <- function(searchConditionsList = NULL, CalculationGroupWeightingID = F, CalculationGroupGradeBucketID = F, CategoryIDToWeight = F, GradingPeriodGradeBucketIDToWeight = F, SubjectIDToWeight = F, AcademicStandardIDToWeight = F, WeightPercent = F, RoundBucketPercent = F, RequiredGrade = F, CalculationGroupWeightingIDClonedFrom = F, PercentEarnedForCategory = F, IsAHistoricRecord = F, AdjustedGradeBucketWeightPercent = F, AdjustedCategoryWeightPercent = F, AdjustedSubjectWeightPercent = F, AdjustedAcademicStandardWeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityGroupKey = F, PointsEarnedForCategory = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupWeighting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CalculationGroupWeighting
	#'
	#' This function returns a dataframe or json object of a CalculationGroupWeighting
	#' @param CalculationGroupWeightingID The ID of the CalculationGroupWeighting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupWeighting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCalculationGroupWeighting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalculationGroupWeighting <- function(CalculationGroupWeightingID, CalculationGroupGradeBucketID = F, CategoryIDToWeight = F, GradingPeriodGradeBucketIDToWeight = F, SubjectIDToWeight = F, AcademicStandardIDToWeight = F, WeightPercent = F, RoundBucketPercent = F, RequiredGrade = F, CalculationGroupWeightingIDClonedFrom = F, PercentEarnedForCategory = F, IsAHistoricRecord = F, AdjustedGradeBucketWeightPercent = F, AdjustedCategoryWeightPercent = F, AdjustedSubjectWeightPercent = F, AdjustedAcademicStandardWeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityGroupKey = F, PointsEarnedForCategory = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalculationGroupWeightingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CalculationGroupWeighting", objectId = CalculationGroupWeightingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseGradingScaleGroupCourses
	#'
	#' This function returns a dataframe or json object of CourseGradingScaleGroupCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseGradingScaleGroupCourse') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CourseGradingScaleGroupCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseGradingScaleGroupCourses <- function(searchConditionsList = NULL, CourseGradingScaleGroupCourseID = F, CourseGradingScaleGroupID = F, CourseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CourseGradingScaleGroupCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseGradingScaleGroupCourse
	#'
	#' This function returns a dataframe or json object of a CourseGradingScaleGroupCourse
	#' @param CourseGradingScaleGroupCourseID The ID of the CourseGradingScaleGroupCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseGradingScaleGroupCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCourseGradingScaleGroupCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseGradingScaleGroupCourse <- function(CourseGradingScaleGroupCourseID, CourseGradingScaleGroupID = F, CourseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseGradingScaleGroupCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CourseGradingScaleGroupCourse", objectId = CourseGradingScaleGroupCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ScoreClarifiers
	#'
	#' This function returns a dataframe or json object of ScoreClarifiers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ScoreClarifier') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of ScoreClarifiers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listScoreClarifiers <- function(searchConditionsList = NULL, ScoreClarifierID = F, Code = F, Description = F, IsMissing = F, NoCount = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, ScoreClarifierIDClonedFrom = F, CodeDescription = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ScoreClarifier", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get ScoreClarifier
	#'
	#' This function returns a dataframe or json object of a ScoreClarifier
	#' @param ScoreClarifierID The ID of the ScoreClarifier to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ScoreClarifier') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getScoreClarifier
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getScoreClarifier <- function(ScoreClarifierID, Code = F, Description = F, IsMissing = F, NoCount = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, ScoreClarifierIDClonedFrom = F, CodeDescription = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ScoreClarifierID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "ScoreClarifier", objectId = ScoreClarifierID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionGradeBuckets
	#'
	#' This function returns a dataframe or json object of SectionGradeBuckets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionGradeBucket') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of SectionGradeBuckets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionGradeBuckets <- function(searchConditionsList = NULL, SectionGradeBucketID = F, SectionID = F, CalculationGroupGradeBucketID = F, IsCalculationTypeOverridden = F, IsOverridden = F, EffectiveCalculationTypeCode = F, HasSectionCategories = F, HasSectionGradeBucketWeights = F, IsAHistoricRecord = F, HasSectionSubjectWeights = F, HasSectionAcademicStandardWeights = F, CalculationModifiedTime = F, CalculationType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasWeightOverride = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionGradeBucket", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionGradeBucket
	#'
	#' This function returns a dataframe or json object of a SectionGradeBucket
	#' @param SectionGradeBucketID The ID of the SectionGradeBucket to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionGradeBucket') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getSectionGradeBucket
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionGradeBucket <- function(SectionGradeBucketID, SectionID = F, CalculationGroupGradeBucketID = F, IsCalculationTypeOverridden = F, IsOverridden = F, EffectiveCalculationTypeCode = F, HasSectionCategories = F, HasSectionGradeBucketWeights = F, IsAHistoricRecord = F, HasSectionSubjectWeights = F, HasSectionAcademicStandardWeights = F, CalculationModifiedTime = F, CalculationType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasWeightOverride = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionGradeBucketID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "SectionGradeBucket", objectId = SectionGradeBucketID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionGradingScaleGroups
	#'
	#' This function returns a dataframe or json object of SectionGradingScaleGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionGradingScaleGroup') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of SectionGradingScaleGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionGradingScaleGroups <- function(searchConditionsList = NULL, SectionGradingScaleGroupID = F, SectionID = F, GradingPeriodGradeBucketID = F, SectionGradingScaleGroupIDClonedFrom = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GradingScaleGroupID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionGradingScaleGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionGradingScaleGroup
	#'
	#' This function returns a dataframe or json object of a SectionGradingScaleGroup
	#' @param SectionGradingScaleGroupID The ID of the SectionGradingScaleGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionGradingScaleGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getSectionGradingScaleGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionGradingScaleGroup <- function(SectionGradingScaleGroupID, SectionID = F, GradingPeriodGradeBucketID = F, SectionGradingScaleGroupIDClonedFrom = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GradingScaleGroupID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionGradingScaleGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "SectionGradingScaleGroup", objectId = SectionGradingScaleGroupID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalculationGroups
	#'
	#' This function returns a dataframe or json object of CalculationGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroup') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CalculationGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalculationGroups <- function(searchConditionsList = NULL, CalculationGroupID = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, Description = F, Filter = F, AllowCategoryOverride = F, Rank = F, IsDefaultGroup = F, CalculationGroupIDClonedFrom = F, LimitTeacherOverridesToCategoriesInGroup = F, IsAHistoricRecord = F, HasSubjects = F, HasAcademicStandards = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowAssignmentPointsTeacherOverride = F, AllowCategoryWeightingTeacherOverride = F, AllowGradeBucketWeightingTeacherOverride = F, AllowTotalPointsAndGradeBucketWeightingTeacherOverride = F, AllowNotGradedTeacherOverride = F, AllowSubjectiveTeacherOverride = F, AllowSubjectWeightingTeacherOverride = F, AllowAcademicStandardWeightingTeacherOverride = F, HasAssignmentPointsOverrides = F, HasCategoryWeightingOverrides = F, HasGradeBucketWeightingOverrides = F, HasTotalPointsAndGradeBucketWeightingOverrides = F, HasNotGradedOverrides = F, HasSubjectiveOverrides = F, HasSubjectWeightingOverrides = F, HasAcademicStandardWeightingOverrides = F, AllowDecayingAverageTeacherOverride = F, DecayingAveragePercent = F, HasDecayingAverageOverrides = F, HasDecayingAverage = F, AllowAssignmentAveragePercentTeacherOverride = F, HasAssignmentAveragePercentOverrides = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CalculationGroup
	#'
	#' This function returns a dataframe or json object of a CalculationGroup
	#' @param CalculationGroupID The ID of the CalculationGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCalculationGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalculationGroup <- function(CalculationGroupID, EntityID = F, SchoolYearID = F, EntityGroupKey = F, Description = F, Filter = F, AllowCategoryOverride = F, Rank = F, IsDefaultGroup = F, CalculationGroupIDClonedFrom = F, LimitTeacherOverridesToCategoriesInGroup = F, IsAHistoricRecord = F, HasSubjects = F, HasAcademicStandards = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowAssignmentPointsTeacherOverride = F, AllowCategoryWeightingTeacherOverride = F, AllowGradeBucketWeightingTeacherOverride = F, AllowTotalPointsAndGradeBucketWeightingTeacherOverride = F, AllowNotGradedTeacherOverride = F, AllowSubjectiveTeacherOverride = F, AllowSubjectWeightingTeacherOverride = F, AllowAcademicStandardWeightingTeacherOverride = F, HasAssignmentPointsOverrides = F, HasCategoryWeightingOverrides = F, HasGradeBucketWeightingOverrides = F, HasTotalPointsAndGradeBucketWeightingOverrides = F, HasNotGradedOverrides = F, HasSubjectiveOverrides = F, HasSubjectWeightingOverrides = F, HasAcademicStandardWeightingOverrides = F, AllowDecayingAverageTeacherOverride = F, DecayingAveragePercent = F, HasDecayingAverageOverrides = F, HasDecayingAverage = F, AllowAssignmentAveragePercentTeacherOverride = F, HasAssignmentAveragePercentOverrides = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalculationGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CalculationGroup", objectId = CalculationGroupID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseGradingScaleGroups
	#'
	#' This function returns a dataframe or json object of CourseGradingScaleGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseGradingScaleGroup') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CourseGradingScaleGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseGradingScaleGroups <- function(searchConditionsList = NULL, CourseGradingScaleGroupID = F, EntityID = F, SchoolYearID = F, Filter = F, Description = F, EntityGroupKey = F, Rank = F, AllowSectionOverride = F, IsDefaultGroup = F, CourseGradingScaleGroupIDClonedFrom = F, LimitTeacherOverridesToGradeMarksInGroup = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GradingScaleGroupID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CourseGradingScaleGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseGradingScaleGroup
	#'
	#' This function returns a dataframe or json object of a CourseGradingScaleGroup
	#' @param CourseGradingScaleGroupID The ID of the CourseGradingScaleGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseGradingScaleGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCourseGradingScaleGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseGradingScaleGroup <- function(CourseGradingScaleGroupID, EntityID = F, SchoolYearID = F, Filter = F, Description = F, EntityGroupKey = F, Rank = F, AllowSectionOverride = F, IsDefaultGroup = F, CourseGradingScaleGroupIDClonedFrom = F, LimitTeacherOverridesToGradeMarksInGroup = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GradingScaleGroupID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseGradingScaleGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CourseGradingScaleGroup", objectId = CourseGradingScaleGroupID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GradingScaleGroupGradeMarks
	#'
	#' This function returns a dataframe or json object of GradingScaleGroupGradeMarks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradingScaleGroupGradeMark') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of GradingScaleGroupGradeMarks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGradingScaleGroupGradeMarks <- function(searchConditionsList = NULL, GradingScaleGroupGradeMarkID = F, GradeMarkID = F, PercentLow = F, PercentHigh = F, DefaultCalculationPercent = F, Color = F, GradingScaleGroupID = F, GradingScaleGroupGradeMarkIDClonedFrom = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowSubjective = F, PointsLow = F, PointsHigh = F, DefaultCalculationPoints = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "GradingScaleGroupGradeMark", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get GradingScaleGroupGradeMark
	#'
	#' This function returns a dataframe or json object of a GradingScaleGroupGradeMark
	#' @param GradingScaleGroupGradeMarkID The ID of the GradingScaleGroupGradeMark to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradingScaleGroupGradeMark') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getGradingScaleGroupGradeMark
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGradingScaleGroupGradeMark <- function(GradingScaleGroupGradeMarkID, GradeMarkID = F, PercentLow = F, PercentHigh = F, DefaultCalculationPercent = F, Color = F, GradingScaleGroupID = F, GradingScaleGroupGradeMarkIDClonedFrom = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowSubjective = F, PointsLow = F, PointsHigh = F, DefaultCalculationPoints = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GradingScaleGroupGradeMarkID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "GradingScaleGroupGradeMark", objectId = GradingScaleGroupGradeMarkID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAssignments
	#'
	#' This function returns a dataframe or json object of StudentAssignments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAssignment') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StudentAssignments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAssignments <- function(searchConditionsList = NULL, StudentAssignmentID = F, StudentSectionID = F, AssignmentID = F, ScoreClarifierID = F, Score = F, Comments = F, IsMissing = F, IsGraded = F, NoCount = F, IsDropped = F, SubmissionStatus = F, SubmissionTime = F, SectionID = F, GradeMarkID = F, PointsEarned = F, PointsEarnedWithSlash = F, PointsEarnedPercent = F, ScoreDisplayValue = F, AllQuestionsScored = F, IsExpired = F, ScoreDisplayValueNoGradeMark = F, IsTransferredScore = F, IsFutureRetainedScore = F, IsScored = F, HasStudentSectionTransaction = F, AssignmentDueDateAttendance = F, StudentQuestionCount = F, HasAnswers = F, AnsweredQuestionCount = F, CurrentQuestionNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentOnlineAssignmentDisplayPointsEarnedWithSlash = F, GradeMarkCode = F, OnlineAssignmentNotificationSent = F, OnlineAssignmentReviewNotificationSent = F, AnswerKeyIsAvailableToView = F, PointsEarnedWithSlashCheckDisplay = F, SubmissionStatusToUse = F, CannotBeTakenByStudent = F, CalculatedPoints = F, ScoreHasChangedWithinSpecifiedAmountOfTime = F, StudentOnlineAssignmentDisplayPointsEarned = F, AttemptCount = F, TimeLastScored = F, IsMissingHasChangedWithinSpecifiedAmountOfTime = F, GradeMarkCodeToUse = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentAssignment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentAssignment
	#'
	#' This function returns a dataframe or json object of a StudentAssignment
	#' @param StudentAssignmentID The ID of the StudentAssignment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAssignment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStudentAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAssignment <- function(StudentAssignmentID, StudentSectionID = F, AssignmentID = F, ScoreClarifierID = F, Score = F, Comments = F, IsMissing = F, IsGraded = F, NoCount = F, IsDropped = F, SubmissionStatus = F, SubmissionTime = F, SectionID = F, GradeMarkID = F, PointsEarned = F, PointsEarnedWithSlash = F, PointsEarnedPercent = F, ScoreDisplayValue = F, AllQuestionsScored = F, IsExpired = F, ScoreDisplayValueNoGradeMark = F, IsTransferredScore = F, IsFutureRetainedScore = F, IsScored = F, HasStudentSectionTransaction = F, AssignmentDueDateAttendance = F, StudentQuestionCount = F, HasAnswers = F, AnsweredQuestionCount = F, CurrentQuestionNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentOnlineAssignmentDisplayPointsEarnedWithSlash = F, GradeMarkCode = F, OnlineAssignmentNotificationSent = F, OnlineAssignmentReviewNotificationSent = F, AnswerKeyIsAvailableToView = F, PointsEarnedWithSlashCheckDisplay = F, SubmissionStatusToUse = F, CannotBeTakenByStudent = F, CalculatedPoints = F, ScoreHasChangedWithinSpecifiedAmountOfTime = F, StudentOnlineAssignmentDisplayPointsEarned = F, AttemptCount = F, TimeLastScored = F, IsMissingHasChangedWithinSpecifiedAmountOfTime = F, GradeMarkCodeToUse = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAssignmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StudentAssignment", objectId = StudentAssignmentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentGradingScaleGroups
	#'
	#' This function returns a dataframe or json object of StudentGradingScaleGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGradingScaleGroup') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StudentGradingScaleGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentGradingScaleGroups <- function(searchConditionsList = NULL, StudentGradingScaleGroupID = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, Description = F, StudentGradingScaleGroupIDClonedFrom = F, StudentGradingScaleGroupStudentSectionCount = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GradingScaleGroupID = F, HasAttachedStudentSections = F, AllowTeachersToModify = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGradingScaleGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentGradingScaleGroup
	#'
	#' This function returns a dataframe or json object of a StudentGradingScaleGroup
	#' @param StudentGradingScaleGroupID The ID of the StudentGradingScaleGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGradingScaleGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStudentGradingScaleGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentGradingScaleGroup <- function(StudentGradingScaleGroupID, EntityID = F, SchoolYearID = F, EntityGroupKey = F, Description = F, StudentGradingScaleGroupIDClonedFrom = F, StudentGradingScaleGroupStudentSectionCount = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GradingScaleGroupID = F, HasAttachedStudentSections = F, AllowTeachersToModify = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentGradingScaleGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StudentGradingScaleGroup", objectId = StudentGradingScaleGroupID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentGradingScaleGroupStudentSections
	#'
	#' This function returns a dataframe or json object of StudentGradingScaleGroupStudentSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGradingScaleGroupStudentSection') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StudentGradingScaleGroupStudentSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentGradingScaleGroupStudentSections <- function(searchConditionsList = NULL, StudentGradingScaleGroupStudentSectionID = F, StudentGradingScaleGroupID = F, StudentSectionID = F, GradeBuckets = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGradingScaleGroupStudentSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentGradingScaleGroupStudentSection
	#'
	#' This function returns a dataframe or json object of a StudentGradingScaleGroupStudentSection
	#' @param StudentGradingScaleGroupStudentSectionID The ID of the StudentGradingScaleGroupStudentSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGradingScaleGroupStudentSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStudentGradingScaleGroupStudentSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentGradingScaleGroupStudentSection <- function(StudentGradingScaleGroupStudentSectionID, StudentGradingScaleGroupID = F, StudentSectionID = F, GradeBuckets = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentGradingScaleGroupStudentSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StudentGradingScaleGroupStudentSection", objectId = StudentGradingScaleGroupStudentSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCalcGroupBucketRemoveStandardOrSubjectResults
	#'
	#' This function returns a dataframe or json object of TempCalcGroupBucketRemoveStandardOrSubjectResults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCalcGroupBucketRemoveStandardOrSubjectResult') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempCalcGroupBucketRemoveStandardOrSubjectResults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCalcGroupBucketRemoveStandardOrSubjectResults <- function(searchConditionsList = NULL, TempCalcGroupBucketRemoveStandardOrSubjectResultID = F, IsError = F, StandardOrSubjectDescription = F, ErrorText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCalcGroupBucketRemoveStandardOrSubjectResult", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempCalcGroupBucketRemoveStandardOrSubjectResult
	#'
	#' This function returns a dataframe or json object of a TempCalcGroupBucketRemoveStandardOrSubjectResult
	#' @param TempCalcGroupBucketRemoveStandardOrSubjectResultID The ID of the TempCalcGroupBucketRemoveStandardOrSubjectResult to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCalcGroupBucketRemoveStandardOrSubjectResult') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempCalcGroupBucketRemoveStandardOrSubjectResult
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCalcGroupBucketRemoveStandardOrSubjectResult <- function(TempCalcGroupBucketRemoveStandardOrSubjectResultID, IsError = F, StandardOrSubjectDescription = F, ErrorText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCalcGroupBucketRemoveStandardOrSubjectResultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempCalcGroupBucketRemoveStandardOrSubjectResult", objectId = TempCalcGroupBucketRemoveStandardOrSubjectResultID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TeacherSectionAcademicStandardGradeBucketSettings
	#'
	#' This function returns a dataframe or json object of TeacherSectionAcademicStandardGradeBucketSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionAcademicStandardGradeBucketSetting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TeacherSectionAcademicStandardGradeBucketSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTeacherSectionAcademicStandardGradeBucketSettings <- function(searchConditionsList = NULL, TeacherSectionAcademicStandardGradeBucketSettingID = F, TeacherSectionSettingID = F, GradingPeriodGradeBucketID = F, AcademicStandardID = F, IsExpanded = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Display = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionAcademicStandardGradeBucketSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TeacherSectionAcademicStandardGradeBucketSetting
	#'
	#' This function returns a dataframe or json object of a TeacherSectionAcademicStandardGradeBucketSetting
	#' @param TeacherSectionAcademicStandardGradeBucketSettingID The ID of the TeacherSectionAcademicStandardGradeBucketSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionAcademicStandardGradeBucketSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTeacherSectionAcademicStandardGradeBucketSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTeacherSectionAcademicStandardGradeBucketSetting <- function(TeacherSectionAcademicStandardGradeBucketSettingID, TeacherSectionSettingID = F, GradingPeriodGradeBucketID = F, AcademicStandardID = F, IsExpanded = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Display = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TeacherSectionAcademicStandardGradeBucketSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TeacherSectionAcademicStandardGradeBucketSetting", objectId = TeacherSectionAcademicStandardGradeBucketSettingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TeacherSectionSubjectGradeBucketSettings
	#'
	#' This function returns a dataframe or json object of TeacherSectionSubjectGradeBucketSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionSubjectGradeBucketSetting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TeacherSectionSubjectGradeBucketSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTeacherSectionSubjectGradeBucketSettings <- function(searchConditionsList = NULL, TeacherSectionSubjectGradeBucketSettingID = F, TeacherSectionSettingID = F, GradingPeriodGradeBucketID = F, SubjectID = F, IsExpanded = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Display = F, AllLinkedAcademicStandardsAreDisplayed = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionSubjectGradeBucketSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TeacherSectionSubjectGradeBucketSetting
	#'
	#' This function returns a dataframe or json object of a TeacherSectionSubjectGradeBucketSetting
	#' @param TeacherSectionSubjectGradeBucketSettingID The ID of the TeacherSectionSubjectGradeBucketSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TeacherSectionSubjectGradeBucketSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTeacherSectionSubjectGradeBucketSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTeacherSectionSubjectGradeBucketSetting <- function(TeacherSectionSubjectGradeBucketSettingID, TeacherSectionSettingID = F, GradingPeriodGradeBucketID = F, SubjectID = F, IsExpanded = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Display = F, AllLinkedAcademicStandardsAreDisplayed = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TeacherSectionSubjectGradeBucketSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TeacherSectionSubjectGradeBucketSetting", objectId = TeacherSectionSubjectGradeBucketSettingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MonitorSummaryByClasses
	#'
	#' This function returns a dataframe or json object of MonitorSummaryByClasses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MonitorSummaryByClass') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of MonitorSummaryByClasses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMonitorSummaryByClasses <- function(searchConditionsList = NULL, MonitorSummaryByClassID = F, LastUpdatedTime = F, StaffMeetID = F, ClosedGradingPeriodGradeChangeCount = F, AssignmentCountYTD = F, StudentCountForTerm = F, AssignmentCountForTerm = F, FutureAssignmentCountForTerm = F, GradedAssignmentCountForTerm = F, NonGradedAssignmentCountForTerm = F, MissingAssignmentCountForTerm = F, NoCountAssignmentCountForTerm = F, ScoreClarifierAssignmentCountForTerm = F, ScoredAssignmentRange100to90CurrentTerm = F, ScoredAssignmentRange89to80CurrentTerm = F, ScoredAssignmentRange79to70CurrentTerm = F, ScoredAssignmentRange69to60CurrentTerm = F, ScoredAssignmentRange59to50CurrentTerm = F, ScoredAssignmentRange49to1CurrentTerm = F, ScoredAssignmentRange0CurrentTerm = F, ExcusedAbsencesYTD = F, UnexcusedAbsencesYTD = F, TardiesYTD = F, OtherAbsencesYTD = F, OffenseCountYTD = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentGradingPeriod = F, LastAccessedGradebookTime = F, LastScoredGradebookTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "MonitorSummaryByClass", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get MonitorSummaryByClass
	#'
	#' This function returns a dataframe or json object of a MonitorSummaryByClass
	#' @param MonitorSummaryByClassID The ID of the MonitorSummaryByClass to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MonitorSummaryByClass') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getMonitorSummaryByClass
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMonitorSummaryByClass <- function(MonitorSummaryByClassID, LastUpdatedTime = F, StaffMeetID = F, ClosedGradingPeriodGradeChangeCount = F, AssignmentCountYTD = F, StudentCountForTerm = F, AssignmentCountForTerm = F, FutureAssignmentCountForTerm = F, GradedAssignmentCountForTerm = F, NonGradedAssignmentCountForTerm = F, MissingAssignmentCountForTerm = F, NoCountAssignmentCountForTerm = F, ScoreClarifierAssignmentCountForTerm = F, ScoredAssignmentRange100to90CurrentTerm = F, ScoredAssignmentRange89to80CurrentTerm = F, ScoredAssignmentRange79to70CurrentTerm = F, ScoredAssignmentRange69to60CurrentTerm = F, ScoredAssignmentRange59to50CurrentTerm = F, ScoredAssignmentRange49to1CurrentTerm = F, ScoredAssignmentRange0CurrentTerm = F, ExcusedAbsencesYTD = F, UnexcusedAbsencesYTD = F, TardiesYTD = F, OtherAbsencesYTD = F, OffenseCountYTD = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentGradingPeriod = F, LastAccessedGradebookTime = F, LastScoredGradebookTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MonitorSummaryByClassID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "MonitorSummaryByClass", objectId = MonitorSummaryByClassID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MonitorSummaryByTeachers
	#'
	#' This function returns a dataframe or json object of MonitorSummaryByTeachers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MonitorSummaryByTeacher') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of MonitorSummaryByTeachers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMonitorSummaryByTeachers <- function(searchConditionsList = NULL, MonitorSummaryByTeacherID = F, LastUpdatedTime = F, StaffID = F, StaffMeetCount = F, ActiveStudentCount = F, AssignmentCountForTerm = F, NonGradedAssignmentCountNoStudentAssignmentsForTerm = F, NonGradedAssignmentCountForTerm = F, GradedAssignmentCountForTerm = F, NoCountAssignmentCountForTerm = F, MissingAssignmentCountForTerm = F, ScoreClarifierAssignmentCountForTerm = F, FutureAssignmentCountForTerm = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SchoolYearID = F, LastAccessedGradebookTime = F, LastAssignmentScoredDueDate = F, LastScoredAssignmentTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "MonitorSummaryByTeacher", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get MonitorSummaryByTeacher
	#'
	#' This function returns a dataframe or json object of a MonitorSummaryByTeacher
	#' @param MonitorSummaryByTeacherID The ID of the MonitorSummaryByTeacher to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MonitorSummaryByTeacher') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getMonitorSummaryByTeacher
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMonitorSummaryByTeacher <- function(MonitorSummaryByTeacherID, LastUpdatedTime = F, StaffID = F, StaffMeetCount = F, ActiveStudentCount = F, AssignmentCountForTerm = F, NonGradedAssignmentCountNoStudentAssignmentsForTerm = F, NonGradedAssignmentCountForTerm = F, GradedAssignmentCountForTerm = F, NoCountAssignmentCountForTerm = F, MissingAssignmentCountForTerm = F, ScoreClarifierAssignmentCountForTerm = F, FutureAssignmentCountForTerm = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SchoolYearID = F, LastAccessedGradebookTime = F, LastAssignmentScoredDueDate = F, LastScoredAssignmentTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MonitorSummaryByTeacherID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "MonitorSummaryByTeacher", objectId = MonitorSummaryByTeacherID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentAttachments
	#'
	#' This function returns a dataframe or json object of AssignmentAttachments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AssignmentAttachment') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of AssignmentAttachments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentAttachments <- function(searchConditionsList = NULL, AssignmentAttachmentID = F, AssignmentID = F, AttachmentID = F, DisplayInStudentFamilyAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "AssignmentAttachment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get AssignmentAttachment
	#'
	#' This function returns a dataframe or json object of an AssignmentAttachment
	#' @param AssignmentAttachmentID The ID of the AssignmentAttachment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AssignmentAttachment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getAssignmentAttachment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentAttachment <- function(AssignmentAttachmentID, AssignmentID = F, AttachmentID = F, DisplayInStudentFamilyAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentAttachmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "AssignmentAttachment", objectId = AssignmentAttachmentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GradebookConfigEntities
	#'
	#' This function returns a dataframe or json object of GradebookConfigEntities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookConfigEntity') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of GradebookConfigEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGradebookConfigEntities <- function(searchConditionsList = NULL, ConfigEntityID = F, EntityID = F, RunInMonitorByScheduledTask = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ConfigEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get GradebookConfigEntity
	#'
	#' This function returns a dataframe or json object of a GradebookConfigEntity
	#' @param GradebookConfigEntityID The ID of the GradebookConfigEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookConfigEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getGradebookConfigEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGradebookConfigEntity <- function(GradebookConfigEntityID, ConfigEntityID = F, EntityID = F, RunInMonitorByScheduledTask = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GradebookConfigEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "ConfigEntity", objectId = GradebookConfigEntityID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GradebookConfigDistricts
	#'
	#' This function returns a dataframe or json object of GradebookConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookConfigDistrict') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of GradebookConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGradebookConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get GradebookConfigDistrict
	#'
	#' This function returns a dataframe or json object of a GradebookConfigDistrict
	#' @param GradebookConfigDistrictID The ID of the GradebookConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getGradebookConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGradebookConfigDistrict <- function(GradebookConfigDistrictID, ConfigDistrictID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GradebookConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "ConfigDistrict", objectId = GradebookConfigDistrictID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSectionAssignments
	#'
	#' This function returns a dataframe or json object of TempSectionAssignments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionAssignment') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempSectionAssignments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSectionAssignments <- function(searchConditionsList = NULL, TempSectionAssignmentID = F, AssignmentID = F, AssignmentName = F, SectionID = F, SectionDetails = F, CurrentPeriod = F, Sync = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CategoryDescription = F, EntityID = F, SchoolYearID = F, AssignedDate = F, DueDate = F, SectionLengthStartDate = F, SectionLengthEndDate = F, MaxScore = F, Weight = F, IsOnlineAssignment = F, StartDate = F, StartTime = F, EndDate = F, EndTime = F, HideScoreUntilDate = F, HideScoreUntilTime = F, ShowCorrectAnswers = F, AnswerRevealDate = F, AnswerRevealTime = F, AttachmentCount = F, CategoryID = F, UseForSpecificGradeBucketType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempSectionAssignment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempSectionAssignment
	#'
	#' This function returns a dataframe or json object of a TempSectionAssignment
	#' @param TempSectionAssignmentID The ID of the TempSectionAssignment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionAssignment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempSectionAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSectionAssignment <- function(TempSectionAssignmentID, AssignmentID = F, AssignmentName = F, SectionID = F, SectionDetails = F, CurrentPeriod = F, Sync = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CategoryDescription = F, EntityID = F, SchoolYearID = F, AssignedDate = F, DueDate = F, SectionLengthStartDate = F, SectionLengthEndDate = F, MaxScore = F, Weight = F, IsOnlineAssignment = F, StartDate = F, StartTime = F, EndDate = F, EndTime = F, HideScoreUntilDate = F, HideScoreUntilTime = F, ShowCorrectAnswers = F, AnswerRevealDate = F, AnswerRevealTime = F, AttachmentCount = F, CategoryID = F, UseForSpecificGradeBucketType = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSectionAssignmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempSectionAssignment", objectId = TempSectionAssignmentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DropLowScoreRunStudentAssignments
	#'
	#' This function returns a dataframe or json object of DropLowScoreRunStudentAssignments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DropLowScoreRunStudentAssignment') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of DropLowScoreRunStudentAssignments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDropLowScoreRunStudentAssignments <- function(searchConditionsList = NULL, DropLowScoreRunStudentAssignmentID = F, DropLowScoreRunID = F, StudentAssignmentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DropActionType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "DropLowScoreRunStudentAssignment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DropLowScoreRunStudentAssignment
	#'
	#' This function returns a dataframe or json object of a DropLowScoreRunStudentAssignment
	#' @param DropLowScoreRunStudentAssignmentID The ID of the DropLowScoreRunStudentAssignment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DropLowScoreRunStudentAssignment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getDropLowScoreRunStudentAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDropLowScoreRunStudentAssignment <- function(DropLowScoreRunStudentAssignmentID, DropLowScoreRunID = F, StudentAssignmentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DropActionType = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DropLowScoreRunStudentAssignmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "DropLowScoreRunStudentAssignment", objectId = DropLowScoreRunStudentAssignmentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DropLowScoreRuns
	#'
	#' This function returns a dataframe or json object of DropLowScoreRuns
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DropLowScoreRun') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of DropLowScoreRuns
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDropLowScoreRuns <- function(searchConditionsList = NULL, DropLowScoreRunID = F, RunTime = F, IsEffective = F, SectionID = F, CalculationGroupGradeBucketID = F, AffectedStudentAssignmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "DropLowScoreRun", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DropLowScoreRun
	#'
	#' This function returns a dataframe or json object of a DropLowScoreRun
	#' @param DropLowScoreRunID The ID of the DropLowScoreRun to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DropLowScoreRun') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getDropLowScoreRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDropLowScoreRun <- function(DropLowScoreRunID, RunTime = F, IsEffective = F, SectionID = F, CalculationGroupGradeBucketID = F, AffectedStudentAssignmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DropLowScoreRunID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "DropLowScoreRun", objectId = DropLowScoreRunID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempGradingPeriodGradeBuckets
	#'
	#' This function returns a dataframe or json object of TempGradingPeriodGradeBuckets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempGradingPeriodGradeBucket') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempGradingPeriodGradeBuckets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempGradingPeriodGradeBuckets <- function(searchConditionsList = NULL, TempGradingPeriodGradeBucketID = F, GradingPeriodGradeBucketID = F, GradeBucketLabelWithDates = F, StartDate = F, EndDate = F, StatusDisplayWithExtensionDate = F, SectionLengthCodeDescription = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempGradingPeriodGradeBucket", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempGradingPeriodGradeBucket
	#'
	#' This function returns a dataframe or json object of a TempGradingPeriodGradeBucket
	#' @param TempGradingPeriodGradeBucketID The ID of the TempGradingPeriodGradeBucket to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempGradingPeriodGradeBucket') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempGradingPeriodGradeBucket
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempGradingPeriodGradeBucket <- function(TempGradingPeriodGradeBucketID, GradingPeriodGradeBucketID = F, GradeBucketLabelWithDates = F, StartDate = F, EndDate = F, StatusDisplayWithExtensionDate = F, SectionLengthCodeDescription = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempGradingPeriodGradeBucketID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempGradingPeriodGradeBucket", objectId = TempGradingPeriodGradeBucketID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempUnDropLowScoreStudentSections
	#'
	#' This function returns a dataframe or json object of TempUnDropLowScoreStudentSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempUnDropLowScoreStudentSection') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempUnDropLowScoreStudentSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempUnDropLowScoreStudentSections <- function(searchConditionsList = NULL, TempUnDropLowScoreStudentSectionID = F, StudentName = F, StudentDisplaySortCode = F, StudentSectionID = F, OriginalGrade = F, NewGrade = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempUnDropLowScoreStudentSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempUnDropLowScoreStudentSection
	#'
	#' This function returns a dataframe or json object of a TempUnDropLowScoreStudentSection
	#' @param TempUnDropLowScoreStudentSectionID The ID of the TempUnDropLowScoreStudentSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempUnDropLowScoreStudentSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempUnDropLowScoreStudentSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempUnDropLowScoreStudentSection <- function(TempUnDropLowScoreStudentSectionID, StudentName = F, StudentDisplaySortCode = F, StudentSectionID = F, OriginalGrade = F, NewGrade = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempUnDropLowScoreStudentSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempUnDropLowScoreStudentSection", objectId = TempUnDropLowScoreStudentSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalculationGroupAcademicStandardCalculationGroupHierarchyDepths
	#'
	#' This function returns a dataframe or json object of CalculationGroupAcademicStandardCalculationGroupHierarchyDepths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupAcademicStandardCalculationGroupHierarchyDepth') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CalculationGroupAcademicStandardCalculationGroupHierarchyDepths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalculationGroupAcademicStandardCalculationGroupHierarchyDepths <- function(searchConditionsList = NULL, CalculationGroupAcademicStandardCalculationGroupHierarchyDepthID = F, EntityGroupKey = F, CalculationGroupAcademicStandardID = F, CalculationGroupHierarchyDepthID = F, CalculationGroupAcademicStandardIDAtLevel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupAcademicStandardCalculationGroupHierarchyDepthIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupAcademicStandardCalculationGroupHierarchyDepth", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CalculationGroupAcademicStandardCalculationGroupHierarchyDepth
	#'
	#' This function returns a dataframe or json object of a CalculationGroupAcademicStandardCalculationGroupHierarchyDepth
	#' @param CalculationGroupAcademicStandardCalculationGroupHierarchyDepthID The ID of the CalculationGroupAcademicStandardCalculationGroupHierarchyDepth to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupAcademicStandardCalculationGroupHierarchyDepth') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCalculationGroupAcademicStandardCalculationGroupHierarchyDepth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalculationGroupAcademicStandardCalculationGroupHierarchyDepth <- function(CalculationGroupAcademicStandardCalculationGroupHierarchyDepthID, EntityGroupKey = F, CalculationGroupAcademicStandardID = F, CalculationGroupHierarchyDepthID = F, CalculationGroupAcademicStandardIDAtLevel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupAcademicStandardCalculationGroupHierarchyDepthIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalculationGroupAcademicStandardCalculationGroupHierarchyDepthID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CalculationGroupAcademicStandardCalculationGroupHierarchyDepth", objectId = CalculationGroupAcademicStandardCalculationGroupHierarchyDepthID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalculationGroupHierarchyDepths
	#'
	#' This function returns a dataframe or json object of CalculationGroupHierarchyDepths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupHierarchyDepth') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of CalculationGroupHierarchyDepths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalculationGroupHierarchyDepths <- function(searchConditionsList = NULL, CalculationGroupHierarchyDepthID = F, EntityGroupKey = F, CalculationGroupID = F, Code = F, Description = F, DepthLevel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupHierarchyDepthIDClonedFrom = F, DynamicRelationshipID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupHierarchyDepth", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CalculationGroupHierarchyDepth
	#'
	#' This function returns a dataframe or json object of a CalculationGroupHierarchyDepth
	#' @param CalculationGroupHierarchyDepthID The ID of the CalculationGroupHierarchyDepth to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CalculationGroupHierarchyDepth') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getCalculationGroupHierarchyDepth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalculationGroupHierarchyDepth <- function(CalculationGroupHierarchyDepthID, EntityGroupKey = F, CalculationGroupID = F, Code = F, Description = F, DepthLevel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupHierarchyDepthIDClonedFrom = F, DynamicRelationshipID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalculationGroupHierarchyDepthID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "CalculationGroupHierarchyDepth", objectId = CalculationGroupHierarchyDepthID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempGradebookGroupErrors
	#'
	#' This function returns a dataframe or json object of TempGradebookGroupErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempGradebookGroupError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempGradebookGroupErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempGradebookGroupErrors <- function(searchConditionsList = NULL, TempGradebookGroupErrorID = F, EntityID = F, CalculationGroupID = F, GradingScaleGroupID = F, Description = F, ErrorText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempGradebookGroupError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempGradebookGroupError
	#'
	#' This function returns a dataframe or json object of a TempGradebookGroupError
	#' @param TempGradebookGroupErrorID The ID of the TempGradebookGroupError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempGradebookGroupError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempGradebookGroupError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempGradebookGroupError <- function(TempGradebookGroupErrorID, EntityID = F, CalculationGroupID = F, GradingScaleGroupID = F, Description = F, ErrorText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempGradebookGroupErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempGradebookGroupError", objectId = TempGradebookGroupErrorID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAssignmentAttempts
	#'
	#' This function returns a dataframe or json object of StudentAssignmentAttempts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAssignmentAttempt') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StudentAssignmentAttempts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAssignmentAttempts <- function(searchConditionsList = NULL, StudentAssignmentAttemptID = F, StudentAssignmentID = F, Score = F, Comments = F, IsUsed = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GradeMarkID = F, DateAttempted = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentAssignmentAttempt", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentAssignmentAttempt
	#'
	#' This function returns a dataframe or json object of a StudentAssignmentAttempt
	#' @param StudentAssignmentAttemptID The ID of the StudentAssignmentAttempt to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAssignmentAttempt') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStudentAssignmentAttempt
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAssignmentAttempt <- function(StudentAssignmentAttemptID, StudentAssignmentID = F, Score = F, Comments = F, IsUsed = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GradeMarkID = F, DateAttempted = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAssignmentAttemptID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StudentAssignmentAttempt", objectId = StudentAssignmentAttemptID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentGroupTeacherSectionSettings
	#'
	#' This function returns a dataframe or json object of StudentGroupTeacherSectionSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGroupTeacherSectionSetting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StudentGroupTeacherSectionSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentGroupTeacherSectionSettings <- function(searchConditionsList = NULL, StudentGroupTeacherSectionSettingID = F, StudentGroupID = F, TeacherSectionSettingID = F, Display = F, Color = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGroupTeacherSectionSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentGroupTeacherSectionSetting
	#'
	#' This function returns a dataframe or json object of a StudentGroupTeacherSectionSetting
	#' @param StudentGroupTeacherSectionSettingID The ID of the StudentGroupTeacherSectionSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGroupTeacherSectionSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStudentGroupTeacherSectionSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentGroupTeacherSectionSetting <- function(StudentGroupTeacherSectionSettingID, StudentGroupID = F, TeacherSectionSettingID = F, Display = F, Color = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentGroupTeacherSectionSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StudentGroupTeacherSectionSetting", objectId = StudentGroupTeacherSectionSettingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentGroupAssignments
	#'
	#' This function returns a dataframe or json object of StudentGroupAssignments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGroupAssignment') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StudentGroupAssignments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentGroupAssignments <- function(searchConditionsList = NULL, StudentGroupAssignmentID = F, StudentGroupID = F, AssignmentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DeleteErrorMessage = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGroupAssignment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentGroupAssignment
	#'
	#' This function returns a dataframe or json object of a StudentGroupAssignment
	#' @param StudentGroupAssignmentID The ID of the StudentGroupAssignment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGroupAssignment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStudentGroupAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentGroupAssignment <- function(StudentGroupAssignmentID, StudentGroupID = F, AssignmentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DeleteErrorMessage = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentGroupAssignmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StudentGroupAssignment", objectId = StudentGroupAssignmentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentGroupStudentSections
	#'
	#' This function returns a dataframe or json object of StudentGroupStudentSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGroupStudentSection') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StudentGroupStudentSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentGroupStudentSections <- function(searchConditionsList = NULL, StudentGroupStudentSectionID = F, StudentGroupID = F, StudentSectionID = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AssignmentCount = F, ScoredAssignmentCount = F, IsActive = F, CanBeDeleted = F, UnableToDeleteMessage = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGroupStudentSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentGroupStudentSection
	#'
	#' This function returns a dataframe or json object of a StudentGroupStudentSection
	#' @param StudentGroupStudentSectionID The ID of the StudentGroupStudentSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGroupStudentSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStudentGroupStudentSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentGroupStudentSection <- function(StudentGroupStudentSectionID, StudentGroupID = F, StudentSectionID = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AssignmentCount = F, ScoredAssignmentCount = F, IsActive = F, CanBeDeleted = F, UnableToDeleteMessage = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentGroupStudentSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StudentGroupStudentSection", objectId = StudentGroupStudentSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentGroups
	#'
	#' This function returns a dataframe or json object of StudentGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGroup') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StudentGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentGroups <- function(searchConditionsList = NULL, StudentGroupID = F, SectionID = F, Name = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, AssignmentCount = F, StudentCount = F, CanBeDeleted = F, StudentGroupSyncKey = F, IsAHistoricRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentGroup
	#'
	#' This function returns a dataframe or json object of a StudentGroup
	#' @param StudentGroupID The ID of the StudentGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStudentGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentGroup <- function(StudentGroupID, SectionID = F, Name = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, AssignmentCount = F, StudentCount = F, CanBeDeleted = F, StudentGroupSyncKey = F, IsAHistoricRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StudentGroup", objectId = StudentGroupID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GradebookVendorCategories
	#'
	#' This function returns a dataframe or json object of GradebookVendorCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookVendorCategory') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of GradebookVendorCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGradebookVendorCategories <- function(searchConditionsList = NULL, VendorCategoryID = F, CategoryID = F, OneRosterVendorID = F, VendorSourceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "VendorCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get GradebookVendorCategory
	#'
	#' This function returns a dataframe or json object of a GradebookVendorCategory
	#' @param GradebookVendorCategoryID The ID of the GradebookVendorCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookVendorCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getGradebookVendorCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGradebookVendorCategory <- function(GradebookVendorCategoryID, VendorCategoryID = F, CategoryID = F, OneRosterVendorID = F, VendorSourceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GradebookVendorCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "VendorCategory", objectId = GradebookVendorCategoryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List VendorStudentAssignments
	#'
	#' This function returns a dataframe or json object of VendorStudentAssignments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('VendorStudentAssignment') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of VendorStudentAssignments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listVendorStudentAssignments <- function(searchConditionsList = NULL, VendorStudentAssignmentID = F, StudentAssignmentID = F, OneRosterVendorID = F, VendorSourceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "VendorStudentAssignment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get VendorStudentAssignment
	#'
	#' This function returns a dataframe or json object of a VendorStudentAssignment
	#' @param VendorStudentAssignmentID The ID of the VendorStudentAssignment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('VendorStudentAssignment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getVendorStudentAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getVendorStudentAssignment <- function(VendorStudentAssignmentID, StudentAssignmentID = F, OneRosterVendorID = F, VendorSourceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "VendorStudentAssignmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "VendorStudentAssignment", objectId = VendorStudentAssignmentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List VendorAssignments
	#'
	#' This function returns a dataframe or json object of VendorAssignments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('VendorAssignment') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of VendorAssignments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listVendorAssignments <- function(searchConditionsList = NULL, VendorAssignmentID = F, AssignmentID = F, OneRosterVendorID = F, VendorSourceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "VendorAssignment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get VendorAssignment
	#'
	#' This function returns a dataframe or json object of a VendorAssignment
	#' @param VendorAssignmentID The ID of the VendorAssignment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('VendorAssignment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getVendorAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getVendorAssignment <- function(VendorAssignmentID, AssignmentID = F, OneRosterVendorID = F, VendorSourceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "VendorAssignmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "VendorAssignment", objectId = VendorAssignmentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentGradingScaleGroupStudentSections
	#'
	#' This function returns a dataframe or json object of TempStudentGradingScaleGroupStudentSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentGradingScaleGroupStudentSection') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempStudentGradingScaleGroupStudentSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentGradingScaleGroupStudentSections <- function(searchConditionsList = NULL, TempStudentGradingScaleGroupStudentSectionID = F, StudentID = F, StudentSectionID = F, SectionLengthID = F, SectionID = F, StudentFullNameLFM = F, CourseDescriptionCodeSectionCode = F, SectionLengthCode = F, HasError = F, ErrorText = F, GradingPeriodGradeBuckets = F, StudentGradingScaleGroupStudentSectionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsSelfPacedSection = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempStudentGradingScaleGroupStudentSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempStudentGradingScaleGroupStudentSection
	#'
	#' This function returns a dataframe or json object of a TempStudentGradingScaleGroupStudentSection
	#' @param TempStudentGradingScaleGroupStudentSectionID The ID of the TempStudentGradingScaleGroupStudentSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentGradingScaleGroupStudentSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempStudentGradingScaleGroupStudentSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentGradingScaleGroupStudentSection <- function(TempStudentGradingScaleGroupStudentSectionID, StudentID = F, StudentSectionID = F, SectionLengthID = F, SectionID = F, StudentFullNameLFM = F, CourseDescriptionCodeSectionCode = F, SectionLengthCode = F, HasError = F, ErrorText = F, GradingPeriodGradeBuckets = F, StudentGradingScaleGroupStudentSectionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsSelfPacedSection = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentGradingScaleGroupStudentSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempStudentGradingScaleGroupStudentSection", objectId = TempStudentGradingScaleGroupStudentSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GradebookConfigDistrictYears
	#'
	#' This function returns a dataframe or json object of GradebookConfigDistrictYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookConfigDistrictYear') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of GradebookConfigDistrictYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGradebookConfigDistrictYears <- function(searchConditionsList = NULL, ConfigDistrictYearID = F, DistrictID = F, SchoolYearID = F, UseCurriculumSubjectsInGradebook = F, ConfigDistrictYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ConfigDistrictYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get GradebookConfigDistrictYear
	#'
	#' This function returns a dataframe or json object of a GradebookConfigDistrictYear
	#' @param GradebookConfigDistrictYearID The ID of the GradebookConfigDistrictYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('GradebookConfigDistrictYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getGradebookConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGradebookConfigDistrictYear <- function(GradebookConfigDistrictYearID, ConfigDistrictYearID = F, DistrictID = F, SchoolYearID = F, UseCurriculumSubjectsInGradebook = F, ConfigDistrictYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GradebookConfigDistrictYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "ConfigDistrictYear", objectId = GradebookConfigDistrictYearID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List UnenteredStudentGradeBuckets
	#'
	#' This function returns a dataframe or json object of UnenteredStudentGradeBuckets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('UnenteredStudentGradeBucket') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of UnenteredStudentGradeBuckets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUnenteredStudentGradeBuckets <- function(searchConditionsList = NULL, StudentGradeBucketID = F, StudentSectionID = F, GradeMarkIDToApply = F, StudentID = F, StaffID = F, EntityID = F, SchoolYearID = F, SectionID = F, GradeBucketID = F, DisplayPeriodID = F, UseServiceIDOverride = F, GradeBucketLabel = F, GradeMarkCode = F, EntityCode = F, CourseDescription = F, Percent = F, StudentGradeBucketFlag = F, IsAdminOverride = F, StudentSectionTransactionStartDate = F, StudentSectionTransactionEndDate = F, GradingPeriodStartDate = F, GradingPeriodEndDate = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "UnenteredStudentGradeBucket", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get UnenteredStudentGradeBucket
	#'
	#' This function returns a dataframe or json object of an UnenteredStudentGradeBucket
	#' @param UnenteredStudentGradeBucketID The ID of the UnenteredStudentGradeBucket to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('UnenteredStudentGradeBucket') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getUnenteredStudentGradeBucket
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUnenteredStudentGradeBucket <- function(UnenteredStudentGradeBucketID, StudentGradeBucketID = F, StudentSectionID = F, GradeMarkIDToApply = F, StudentID = F, StaffID = F, EntityID = F, SchoolYearID = F, SectionID = F, GradeBucketID = F, DisplayPeriodID = F, UseServiceIDOverride = F, GradeBucketLabel = F, GradeMarkCode = F, EntityCode = F, CourseDescription = F, Percent = F, StudentGradeBucketFlag = F, IsAdminOverride = F, StudentSectionTransactionStartDate = F, StudentSectionTransactionEndDate = F, GradingPeriodStartDate = F, GradingPeriodEndDate = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UnenteredStudentGradeBucketID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "UnenteredStudentGradeBucket", objectId = UnenteredStudentGradeBucketID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StaffMeetLastScoreds
	#'
	#' This function returns a dataframe or json object of StaffMeetLastScoreds
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StaffMeetLastScored') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StaffMeetLastScoreds
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStaffMeetLastScoreds <- function(searchConditionsList = NULL, StaffMeetLastScoredID = F, StaffMeetID = F, StudentAssignmentIDLastScored = F, TimeLastScored = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StaffMeetLastScored", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StaffMeetLastScored
	#'
	#' This function returns a dataframe or json object of a StaffMeetLastScored
	#' @param StaffMeetLastScoredID The ID of the StaffMeetLastScored to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StaffMeetLastScored') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStaffMeetLastScored
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStaffMeetLastScored <- function(StaffMeetLastScoredID, StaffMeetID = F, StudentAssignmentIDLastScored = F, TimeLastScored = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StaffMeetLastScoredID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StaffMeetLastScored", objectId = StaffMeetLastScoredID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ContingentStudentAssignments
	#'
	#' This function returns a dataframe or json object of ContingentStudentAssignments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ContingentStudentAssignment') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of ContingentStudentAssignments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listContingentStudentAssignments <- function(searchConditionsList = NULL, StudentSectionID = F, AssignmentID = F, StudentAssignmentID = F, SectionID = F, EntityID = F, SchoolYearID = F, DueDate = F, IsEffectivelyMissing = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ContingentStudentAssignment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get ContingentStudentAssignment
	#'
	#' This function returns a dataframe or json object of a ContingentStudentAssignment
	#' @param ContingentStudentAssignmentID The ID of the ContingentStudentAssignment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ContingentStudentAssignment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getContingentStudentAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getContingentStudentAssignment <- function(ContingentStudentAssignmentID, StudentSectionID = F, AssignmentID = F, StudentAssignmentID = F, SectionID = F, EntityID = F, SchoolYearID = F, DueDate = F, IsEffectivelyMissing = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ContingentStudentAssignmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "ContingentStudentAssignment", objectId = ContingentStudentAssignmentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentAssignmentScores
	#'
	#' This function returns a dataframe or json object of TempStudentAssignmentScores
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentAssignmentScore') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of TempStudentAssignmentScores
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentAssignmentScores <- function(searchConditionsList = NULL, TempStudentAssignmentScoreID = F, StudentAssignmentID = F, SectionID = F, OriginalScore = F, NewScore = F, OriginalGradeMarkID = F, NewGradeMarkID = F, OriginalGradeMarkCode = F, NewGradeMarkCode = F, MaxScore = F, Weight = F, AssignmentName = F, DueDate = F, StudentName = F, ClassName = F, ErrorMessage = F, AttemptCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempStudentAssignmentScore", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempStudentAssignmentScore
	#'
	#' This function returns a dataframe or json object of a TempStudentAssignmentScore
	#' @param TempStudentAssignmentScoreID The ID of the TempStudentAssignmentScore to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentAssignmentScore') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getTempStudentAssignmentScore
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentAssignmentScore <- function(TempStudentAssignmentScoreID, StudentAssignmentID = F, SectionID = F, OriginalScore = F, NewScore = F, OriginalGradeMarkID = F, NewGradeMarkID = F, OriginalGradeMarkCode = F, NewGradeMarkCode = F, MaxScore = F, Weight = F, AssignmentName = F, DueDate = F, StudentName = F, ClassName = F, ErrorMessage = F, AttemptCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentAssignmentScoreID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "TempStudentAssignmentScore", objectId = TempStudentAssignmentScoreID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentGradeBucketTransferRuns
	#'
	#' This function returns a dataframe or json object of StudentGradeBucketTransferRuns
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGradeBucketTransferRun') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A list of StudentGradeBucketTransferRuns
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentGradeBucketTransferRuns <- function(searchConditionsList = NULL, StudentGradeBucketTransferRunID = F, StudentGradeBucketID = F, RunTime = F, Percent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGradeBucketTransferRun", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentGradeBucketTransferRun
	#'
	#' This function returns a dataframe or json object of a StudentGradeBucketTransferRun
	#' @param StudentGradeBucketTransferRunID The ID of the StudentGradeBucketTransferRun to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentGradeBucketTransferRun') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Gradebook
	#' @return A dataframe or of getStudentGradeBucketTransferRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentGradeBucketTransferRun <- function(StudentGradeBucketTransferRunID, StudentGradeBucketID = F, RunTime = F, Percent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentGradeBucketTransferRunID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Gradebook", objectName = "StudentGradeBucketTransferRun", objectId = StudentGradeBucketTransferRunID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}
