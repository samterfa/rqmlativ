
	#' List BlockPeriods
	#'
	#' This function returns a dataframe or json object of BlockPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('BlockPeriod') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of BlockPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBlockPeriods <- function(searchConditionsList = NULL, BlockPeriodID = F, Code = F, BlockPeriodIDClonedFrom = F, BlockPeriodIDClonedTo = F, HasDisplayPeriods = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityID = F, SchoolYearID = F, Description = F, CodeDescription = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "BlockPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get BlockPeriod
	#'
	#' This function returns a dataframe or json object of a BlockPeriod
	#' @param BlockPeriodID The ID of the BlockPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('BlockPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getBlockPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBlockPeriod <- function(BlockPeriodID, Code = F, BlockPeriodIDClonedFrom = F, BlockPeriodIDClonedTo = F, HasDisplayPeriods = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityID = F, SchoolYearID = F, Description = F, CodeDescription = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BlockPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "BlockPeriod", objectId = BlockPeriodID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BlockPeriodDisplayPeriods
	#'
	#' This function returns a dataframe or json object of BlockPeriodDisplayPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('BlockPeriodDisplayPeriod') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of BlockPeriodDisplayPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBlockPeriodDisplayPeriods <- function(searchConditionsList = NULL, BlockPeriodDisplayPeriodID = F, BlockPeriodID = F, DisplayPeriodID = F, BlockPeriodDisplayPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "BlockPeriodDisplayPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get BlockPeriodDisplayPeriod
	#'
	#' This function returns a dataframe or json object of a BlockPeriodDisplayPeriod
	#' @param BlockPeriodDisplayPeriodID The ID of the BlockPeriodDisplayPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('BlockPeriodDisplayPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getBlockPeriodDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBlockPeriodDisplayPeriod <- function(BlockPeriodDisplayPeriodID, BlockPeriodID = F, DisplayPeriodID = F, BlockPeriodDisplayPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BlockPeriodDisplayPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "BlockPeriodDisplayPeriod", objectId = BlockPeriodDisplayPeriodID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MaximumTeachingHourOverrides
	#'
	#' This function returns a dataframe or json object of MaximumTeachingHourOverrides
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MaximumTeachingHourOverride') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of MaximumTeachingHourOverrides
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMaximumTeachingHourOverrides <- function(searchConditionsList = NULL, MaximumTeachingHourOverrideID = F, StaffEntityYearID = F, DayRotationID = F, MaximumConsecutiveTeachingHours = F, MaximumTotalTeachingHours = F, MaximumTeachingHourOverrideIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MaximumTeachingHourOverride", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get MaximumTeachingHourOverride
	#'
	#' This function returns a dataframe or json object of a MaximumTeachingHourOverride
	#' @param MaximumTeachingHourOverrideID The ID of the MaximumTeachingHourOverride to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MaximumTeachingHourOverride') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getMaximumTeachingHourOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMaximumTeachingHourOverride <- function(MaximumTeachingHourOverrideID, StaffEntityYearID = F, DayRotationID = F, MaximumConsecutiveTeachingHours = F, MaximumTotalTeachingHours = F, MaximumTeachingHourOverrideIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MaximumTeachingHourOverrideID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "MaximumTeachingHourOverride", objectId = MaximumTeachingHourOverrideID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSchedulerDisplayPeriodExcludedForCourses
	#'
	#' This function returns a dataframe or json object of SectionSchedulerDisplayPeriodExcludedForCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerDisplayPeriodExcludedForCourse') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionSchedulerDisplayPeriodExcludedForCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSchedulerDisplayPeriodExcludedForCourses <- function(searchConditionsList = NULL, SectionSchedulerDisplayPeriodExcludedForCourseID = F, CourseID = F, DisplayPeriodID = F, SectionSchedulerDisplayPeriodExcludedForCourseIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerDisplayPeriodExcludedForCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSchedulerDisplayPeriodExcludedForCourse
	#'
	#' This function returns a dataframe or json object of a SectionSchedulerDisplayPeriodExcludedForCourse
	#' @param SectionSchedulerDisplayPeriodExcludedForCourseID The ID of the SectionSchedulerDisplayPeriodExcludedForCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerDisplayPeriodExcludedForCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionSchedulerDisplayPeriodExcludedForCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSchedulerDisplayPeriodExcludedForCourse <- function(SectionSchedulerDisplayPeriodExcludedForCourseID, CourseID = F, DisplayPeriodID = F, SectionSchedulerDisplayPeriodExcludedForCourseIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSchedulerDisplayPeriodExcludedForCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionSchedulerDisplayPeriodExcludedForCourse", objectId = SectionSchedulerDisplayPeriodExcludedForCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseEntityOfferedToSectionMeetDisplayPeriods
	#'
	#' This function returns a dataframe or json object of CourseEntityOfferedToSectionMeetDisplayPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseEntityOfferedToSectionMeetDisplayPeriod') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseEntityOfferedToSectionMeetDisplayPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseEntityOfferedToSectionMeetDisplayPeriods <- function(searchConditionsList = NULL, CourseEntityOfferedToSectionMeetDisplayPeriodID = F, CourseEntityOfferedToSectionID = F, MeetID = F, DisplayPeriodID = F, HideMeetDisplayPeriod = F, IsPrimary = F, CourseEntityOfferedToSectionMeetDisplayPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseEntityOfferedToSectionMeetDisplayPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseEntityOfferedToSectionMeetDisplayPeriod
	#'
	#' This function returns a dataframe or json object of a CourseEntityOfferedToSectionMeetDisplayPeriod
	#' @param CourseEntityOfferedToSectionMeetDisplayPeriodID The ID of the CourseEntityOfferedToSectionMeetDisplayPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseEntityOfferedToSectionMeetDisplayPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseEntityOfferedToSectionMeetDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseEntityOfferedToSectionMeetDisplayPeriod <- function(CourseEntityOfferedToSectionMeetDisplayPeriodID, CourseEntityOfferedToSectionID = F, MeetID = F, DisplayPeriodID = F, HideMeetDisplayPeriod = F, IsPrimary = F, CourseEntityOfferedToSectionMeetDisplayPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseEntityOfferedToSectionMeetDisplayPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseEntityOfferedToSectionMeetDisplayPeriod", objectId = CourseEntityOfferedToSectionMeetDisplayPeriodID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseEntityOfferedTos
	#'
	#' This function returns a dataframe or json object of CourseEntityOfferedTos
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseEntityOfferedTo') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseEntityOfferedTos
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseEntityOfferedTos <- function(searchConditionsList = NULL, CourseEntityOfferedToID = F, CourseID = F, CourseCode = F, EntityIDOfferedTo = F, SchoolYearID = F, UseIsRequiredOverride = F, IsRequiredOverride = F, UseSchedulingPriorityOverride = F, SchedulingPriorityOverride = F, UseSchedulingTypeOverride = F, SchedulingTypeOverride = F, CourseEntityOfferedToIDClonedFrom = F, CourseEntityOfferedToIDClonedTo = F, IsAutoOffering = F, ViewingFromOfferedEntity = F, ViewingFromOfferingEntity = F, IsOfferedCourse = F, IsHomeCourse = F, IsRequired = F, SchedulingTypeCode = F, SchedulingPriorityCode = F, ActiveSections = F, ActiveSectionsOpen = F, TotalSeatsAvailable = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Required = F, NumberOfAlternateCourseRequestsEntity = F, NumberOfCourseRequestsEntity = F, NumberOfCourseRequestsFemalesEntity = F, NumberOfCourseRequestsMalesEntity = F, HasStudentCourseRequestsEntity = F, IsCurrentSchoolYearEntity = F, NumberOfSeatsRemainingEntity = F, NumberTransferStudentSectionsEntity = F, TotalSectionCountEntity = F, TotalStudentCourseRequestSectionLengthSubsetCountEntity = F, TotalStudentSectionCountEntity = F, IsActive = F, EntityGroupKey = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseEntityOfferedTo", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseEntityOfferedTo
	#'
	#' This function returns a dataframe or json object of a CourseEntityOfferedTo
	#' @param CourseEntityOfferedToID The ID of the CourseEntityOfferedTo to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseEntityOfferedTo') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseEntityOfferedTo
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseEntityOfferedTo <- function(CourseEntityOfferedToID, CourseID = F, CourseCode = F, EntityIDOfferedTo = F, SchoolYearID = F, UseIsRequiredOverride = F, IsRequiredOverride = F, UseSchedulingPriorityOverride = F, SchedulingPriorityOverride = F, UseSchedulingTypeOverride = F, SchedulingTypeOverride = F, CourseEntityOfferedToIDClonedFrom = F, CourseEntityOfferedToIDClonedTo = F, IsAutoOffering = F, ViewingFromOfferedEntity = F, ViewingFromOfferingEntity = F, IsOfferedCourse = F, IsHomeCourse = F, IsRequired = F, SchedulingTypeCode = F, SchedulingPriorityCode = F, ActiveSections = F, ActiveSectionsOpen = F, TotalSeatsAvailable = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Required = F, NumberOfAlternateCourseRequestsEntity = F, NumberOfCourseRequestsEntity = F, NumberOfCourseRequestsFemalesEntity = F, NumberOfCourseRequestsMalesEntity = F, HasStudentCourseRequestsEntity = F, IsCurrentSchoolYearEntity = F, NumberOfSeatsRemainingEntity = F, NumberTransferStudentSectionsEntity = F, TotalSectionCountEntity = F, TotalStudentCourseRequestSectionLengthSubsetCountEntity = F, TotalStudentSectionCountEntity = F, IsActive = F, EntityGroupKey = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseEntityOfferedToID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseEntityOfferedTo", objectId = CourseEntityOfferedToID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseEntityOfferedToSectionStaffMeets
	#'
	#' This function returns a dataframe or json object of CourseEntityOfferedToSectionStaffMeets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseEntityOfferedToSectionStaffMeet') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseEntityOfferedToSectionStaffMeets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseEntityOfferedToSectionStaffMeets <- function(searchConditionsList = NULL, CourseEntityOfferedToSectionStaffMeetID = F, CourseEntityOfferedToSectionID = F, MeetID = F, StaffID = F, CourseEntityOfferedToSectionStaffMeetIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseEntityOfferedToSectionStaffMeet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseEntityOfferedToSectionStaffMeet
	#'
	#' This function returns a dataframe or json object of a CourseEntityOfferedToSectionStaffMeet
	#' @param CourseEntityOfferedToSectionStaffMeetID The ID of the CourseEntityOfferedToSectionStaffMeet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseEntityOfferedToSectionStaffMeet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseEntityOfferedToSectionStaffMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseEntityOfferedToSectionStaffMeet <- function(CourseEntityOfferedToSectionStaffMeetID, CourseEntityOfferedToSectionID = F, MeetID = F, StaffID = F, CourseEntityOfferedToSectionStaffMeetIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseEntityOfferedToSectionStaffMeetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseEntityOfferedToSectionStaffMeet", objectId = CourseEntityOfferedToSectionStaffMeetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseEntityOfferedToSections
	#'
	#' This function returns a dataframe or json object of CourseEntityOfferedToSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseEntityOfferedToSection') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseEntityOfferedToSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseEntityOfferedToSections <- function(searchConditionsList = NULL, CourseEntityOfferedToSectionID = F, CourseEntityOfferedToID = F, SectionID = F, MaximumStudentCount = F, IsActive = F, HasMeetDisplayPeriodOverrides = F, CourseEntityOfferedToSectionIDClonedFrom = F, CourseEntityOfferedToSectionIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalEnrollmentCountEntity = F, StudentEnrollmentEntity = F, SeatsAvailableEntity = F, CourseID = F, EntityIDOfferedTo = F, SchoolYearID = F, ViewingFromOfferedEntity = F, ViewingFromOfferingEntity = F, RoomSeatsAvailable = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseEntityOfferedToSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseEntityOfferedToSection
	#'
	#' This function returns a dataframe or json object of a CourseEntityOfferedToSection
	#' @param CourseEntityOfferedToSectionID The ID of the CourseEntityOfferedToSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseEntityOfferedToSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseEntityOfferedToSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseEntityOfferedToSection <- function(CourseEntityOfferedToSectionID, CourseEntityOfferedToID = F, SectionID = F, MaximumStudentCount = F, IsActive = F, HasMeetDisplayPeriodOverrides = F, CourseEntityOfferedToSectionIDClonedFrom = F, CourseEntityOfferedToSectionIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalEnrollmentCountEntity = F, StudentEnrollmentEntity = F, SeatsAvailableEntity = F, CourseID = F, EntityIDOfferedTo = F, SchoolYearID = F, ViewingFromOfferedEntity = F, ViewingFromOfferingEntity = F, RoomSeatsAvailable = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseEntityOfferedToSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseEntityOfferedToSection", objectId = CourseEntityOfferedToSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseEntityOfferedToSectionMeets
	#'
	#' This function returns a dataframe or json object of CourseEntityOfferedToSectionMeets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseEntityOfferedToSectionMeet') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseEntityOfferedToSectionMeets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseEntityOfferedToSectionMeets <- function(searchConditionsList = NULL, CourseEntityOfferedToSectionMeetID = F, CourseEntityOfferedToSectionID = F, MeetID = F, RoomID = F, CourseEntityOfferedToSectionMeetIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionID = F, EntityIDOfferedTo = F, SchoolYearID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseEntityOfferedToSectionMeet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseEntityOfferedToSectionMeet
	#'
	#' This function returns a dataframe or json object of a CourseEntityOfferedToSectionMeet
	#' @param CourseEntityOfferedToSectionMeetID The ID of the CourseEntityOfferedToSectionMeet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseEntityOfferedToSectionMeet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseEntityOfferedToSectionMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseEntityOfferedToSectionMeet <- function(CourseEntityOfferedToSectionMeetID, CourseEntityOfferedToSectionID = F, MeetID = F, RoomID = F, CourseEntityOfferedToSectionMeetIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionID = F, EntityIDOfferedTo = F, SchoolYearID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseEntityOfferedToSectionMeetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseEntityOfferedToSectionMeet", objectId = CourseEntityOfferedToSectionMeetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseLevelMNS
	#'
	#' This function returns a dataframe or json object of CourseLevelMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseLevelMN') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseLevelMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseLevelMNS <- function(searchConditionsList = NULL, CourseLevelMNID = F, Code = F, StateCollegeCourseLevelMNID = F, Credits = F, Title = F, CurriculumYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InstitutionID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseLevelMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseLevelMN
	#'
	#' This function returns a dataframe or json object of a CourseLevelMN
	#' @param CourseLevelMNID The ID of the CourseLevelMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseLevelMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseLevelMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseLevelMN <- function(CourseLevelMNID, Code = F, StateCollegeCourseLevelMNID = F, Credits = F, Title = F, CurriculumYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InstitutionID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseLevelMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseLevelMN", objectId = CourseLevelMNID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MCCCTermGradeBucketMNS
	#'
	#' This function returns a dataframe or json object of MCCCTermGradeBucketMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MCCCTermGradeBucketMN') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of MCCCTermGradeBucketMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMCCCTermGradeBucketMNS <- function(searchConditionsList = NULL, MCCCTermGradeBucketMNID = F, SectionLengthID = F, MCCCTermImportID = F, GradeBucketID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MCCCTermGradeBucketMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get MCCCTermGradeBucketMN
	#'
	#' This function returns a dataframe or json object of a MCCCTermGradeBucketMN
	#' @param MCCCTermGradeBucketMNID The ID of the MCCCTermGradeBucketMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MCCCTermGradeBucketMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getMCCCTermGradeBucketMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMCCCTermGradeBucketMN <- function(MCCCTermGradeBucketMNID, SectionLengthID = F, MCCCTermImportID = F, GradeBucketID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MCCCTermGradeBucketMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "MCCCTermGradeBucketMN", objectId = MCCCTermGradeBucketMNID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchedulingCourses
	#'
	#' This function returns a dataframe or json object of SchedulingCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingCourse') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SchedulingCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchedulingCourses <- function(searchConditionsList = NULL, CourseMNID = F, SequenceNumber = F, SequenceLimit = F, IsDirectPay = F, CourseID = F, CourseCode = F, Category = F, Description = F, CurriculumID = F, IsActive = F, SchoolYearID = F, HideFromArenaScheduling = F, EarnedCredits = F, CourseLengthID = F, IsRequired = F, EntityID = F, EntityGroupKey = F, DepartmentID = F, CourseTypeID = F, CourseSubjectID = F, ActivityID = F, IsRepeatable = F, IsCoreAcademic = F, HideFromRequestEntry = F, IsHonors = F, IsWritingEmphasis = F, PreventDrop = F, Website = F, SectionSchedulerManualProcessingOrder = F, PreventMultipleSectionsUsingSingleDisplayPeriod = F, MaximumPercentageOfSectionsInSingleDisplayPeriod = F, LockCourseFromSectionScheduler = F, OverrideStudentSectionLinkByCourse = F, SchedulingPriority = F, SchedulingType = F, SchedulingTeamMode = F, CourseIDClonedFrom = F, CourseCloned = F, AcademicMinutes = F, KeepAttendance = F, GradeCourse = F, EstimatedNumberOfSections = F, EdFiCourseLevelCharacteristicID = F, EdFiSubjectTypeID = F, CodeDescription = F, HasSubjects = F, HasAttachedStandards = F, CourseIDClonedTo = F, IsAHistoricRecord = F, GradingPeriodSetID = F, RequestLimitPerStudent = F, GradeLevelSummary = F, Required = F, NumberOfAlternateCourseRequests = F, NumberOfCourseRequests = F, NumberOfCourseRequestsFemales = F, NumberOfCourseRequestsMales = F, HasNonAlternateStudentCourseRequests = F, NumberOfCourseRequestsInCommonWithCourse = F, HasCourseRequestsInCommonWithCourse = F, ActiveSections = F, ActiveSectionsOpen = F, IsCurrentSchoolYear = F, NumberOfSeatsAvailable = F, NumberOfSeatsRemaining = F, NumberOfTransferStudentSections = F, SpecificStudentRequests = F, SumTotalActiveSectionsOptimalStudentCount = F, TotalSectionCount = F, TotalStudentCourseRequestSectionLengthSubsetCount = F, TotalStudentSectionCount = F, ViewingFromOfferingEntity = F, HasOfferedCourseEntity = F, IsOffered = F, OfferingEntity = F, TotalEntitiesOfferedTo = F, UseRequiredOverride = F, IsRequiredOverride = F, UseSchedulingTypeOverride = F, SchedulingTypeCodeOverride = F, SchedulingTypeOverrideCode = F, UseSchedulingPriorityOverride = F, SchedulingPriorityCodeOverride = F, SchedulingPriorityOverrideCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateSTARAssignmentCodeMNID = F, LengthOfPeriod = F, StateSTARGradeLevelMNID = F, PeriodsPerWeek = F, ExcludeFromSTAR = F, StateCarlPerkinsProgramCodeMNID = F, IsActiveForEntity = F, CanBeOfferedToAnotherEntity = F, GradeLevelIDSummary = F, AllowTeachersToAddAssignments = F, CourseGradeLevelCodes = F, CourseGroupDescriptions = F, ExcludeFromStudentSectionLink = F, HasStudentCourseRequests = F, MaxRequestedCount = F, CourseFeesList = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "Course", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SchedulingCourse
	#'
	#' This function returns a dataframe or json object of a SchedulingCourse
	#' @param SchedulingCourseID The ID of the SchedulingCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSchedulingCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchedulingCourse <- function(SchedulingCourseID, CourseMNID = F, SequenceNumber = F, SequenceLimit = F, IsDirectPay = F, CourseID = F, CourseCode = F, Category = F, Description = F, CurriculumID = F, IsActive = F, SchoolYearID = F, HideFromArenaScheduling = F, EarnedCredits = F, CourseLengthID = F, IsRequired = F, EntityID = F, EntityGroupKey = F, DepartmentID = F, CourseTypeID = F, CourseSubjectID = F, ActivityID = F, IsRepeatable = F, IsCoreAcademic = F, HideFromRequestEntry = F, IsHonors = F, IsWritingEmphasis = F, PreventDrop = F, Website = F, SectionSchedulerManualProcessingOrder = F, PreventMultipleSectionsUsingSingleDisplayPeriod = F, MaximumPercentageOfSectionsInSingleDisplayPeriod = F, LockCourseFromSectionScheduler = F, OverrideStudentSectionLinkByCourse = F, SchedulingPriority = F, SchedulingType = F, SchedulingTeamMode = F, CourseIDClonedFrom = F, CourseCloned = F, AcademicMinutes = F, KeepAttendance = F, GradeCourse = F, EstimatedNumberOfSections = F, EdFiCourseLevelCharacteristicID = F, EdFiSubjectTypeID = F, CodeDescription = F, HasSubjects = F, HasAttachedStandards = F, CourseIDClonedTo = F, IsAHistoricRecord = F, GradingPeriodSetID = F, RequestLimitPerStudent = F, GradeLevelSummary = F, Required = F, NumberOfAlternateCourseRequests = F, NumberOfCourseRequests = F, NumberOfCourseRequestsFemales = F, NumberOfCourseRequestsMales = F, HasNonAlternateStudentCourseRequests = F, NumberOfCourseRequestsInCommonWithCourse = F, HasCourseRequestsInCommonWithCourse = F, ActiveSections = F, ActiveSectionsOpen = F, IsCurrentSchoolYear = F, NumberOfSeatsAvailable = F, NumberOfSeatsRemaining = F, NumberOfTransferStudentSections = F, SpecificStudentRequests = F, SumTotalActiveSectionsOptimalStudentCount = F, TotalSectionCount = F, TotalStudentCourseRequestSectionLengthSubsetCount = F, TotalStudentSectionCount = F, ViewingFromOfferingEntity = F, HasOfferedCourseEntity = F, IsOffered = F, OfferingEntity = F, TotalEntitiesOfferedTo = F, UseRequiredOverride = F, IsRequiredOverride = F, UseSchedulingTypeOverride = F, SchedulingTypeCodeOverride = F, SchedulingTypeOverrideCode = F, UseSchedulingPriorityOverride = F, SchedulingPriorityCodeOverride = F, SchedulingPriorityOverrideCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateSTARAssignmentCodeMNID = F, LengthOfPeriod = F, StateSTARGradeLevelMNID = F, PeriodsPerWeek = F, ExcludeFromSTAR = F, StateCarlPerkinsProgramCodeMNID = F, IsActiveForEntity = F, CanBeOfferedToAnotherEntity = F, GradeLevelIDSummary = F, AllowTeachersToAddAssignments = F, CourseGradeLevelCodes = F, CourseGroupDescriptions = F, ExcludeFromStudentSectionLink = F, HasStudentCourseRequests = F, MaxRequestedCount = F, CourseFeesList = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchedulingCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "Course", objectId = SchedulingCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionLengthSubsets
	#'
	#' This function returns a dataframe or json object of SectionLengthSubsets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionLengthSubset') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionLengthSubsets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionLengthSubsets <- function(searchConditionsList = NULL, SectionLengthSubsetMNID = F, MCCCDropDate = F, MCCCTermImportID = F, SectionLengthSubsetID = F, SectionLengthID = F, Code = F, Description = F, IsFullSectionLength = F, StartDate = F, EndDate = F, EntityGroupKey = F, SectionLengthSubsetIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EdFiTermTypeID = F, GradeBucketIDCarlPerkins = F, SectionLengthSubsetIDClonedTo = F, EdFiTermDescriptorID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionLengthSubset", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionLengthSubset
	#'
	#' This function returns a dataframe or json object of a SectionLengthSubset
	#' @param SectionLengthSubsetID The ID of the SectionLengthSubset to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionLengthSubset') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionLengthSubset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionLengthSubset <- function(SectionLengthSubsetID, SectionLengthSubsetMNID = F, MCCCDropDate = F, MCCCTermImportID = F, SectionLengthID = F, Code = F, Description = F, IsFullSectionLength = F, StartDate = F, EndDate = F, EntityGroupKey = F, SectionLengthSubsetIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EdFiTermTypeID = F, GradeBucketIDCarlPerkins = F, SectionLengthSubsetIDClonedTo = F, EdFiTermDescriptorID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionLengthSubsetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionLengthSubset", objectId = SectionLengthSubsetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionLengths
	#'
	#' This function returns a dataframe or json object of SectionLengths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionLength') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionLengths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionLengths <- function(searchConditionsList = NULL, SectionLengthMNID = F, MCCCDropDate = F, SectionLengthID = F, CourseLengthID = F, Code = F, Description = F, EntityGroupKey = F, StartDate = F, EndDate = F, SectionLengthIDClonedFrom = F, EdFiTermTypeID = F, CodeDescription = F, SectionRange = F, IsSectionCurrent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionLengthIDClonedTo = F, ThirdPartyTermTypeOverride = F, ThirdPartyTermNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionLength", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionLength
	#'
	#' This function returns a dataframe or json object of a SectionLength
	#' @param SectionLengthID The ID of the SectionLength to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionLength') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionLength
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionLength <- function(SectionLengthID, SectionLengthMNID = F, MCCCDropDate = F, CourseLengthID = F, Code = F, Description = F, EntityGroupKey = F, StartDate = F, EndDate = F, SectionLengthIDClonedFrom = F, EdFiTermTypeID = F, CodeDescription = F, SectionRange = F, IsSectionCurrent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionLengthIDClonedTo = F, ThirdPartyTermTypeOverride = F, ThirdPartyTermNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionLengthID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionLength", objectId = SectionLengthID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Sections
	#'
	#' This function returns a dataframe or json object of Sections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Section') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of Sections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSections <- function(searchConditionsList = NULL, SectionMNID = F, LanguageID = F, StateInstructionalMethodCodeMNID = F, CalendarIDMCCCOverride = F, SectionID = F, CourseID = F, EdFiEducationalEnvironmentID = F, AllowCECE = F, Code = F, IsActive = F, SectionLengthID = F, SpecialEdPercentageLimit = F, IsBilingual = F, Website = F, MinimumStudentCount = F, MaximumStudentCount = F, OptimalStudentCount = F, ReservedSeatCount = F, RecalculateGradebook = F, RecalculateGradebookAdmin = F, AllowStudentsWithoutCategoryToBeAssigned = F, SectionIDClonedFrom = F, HomeroomID = F, FitCode = F, IsSelfPaced = F, IsAHistoricRecord = F, TotalMeetCount = F, CanAddStudentSection = F, SchedulingCategories = F, SchedulingTeams = F, CourseCodeSectionCode = F, CourseDescriptionCodeSectionCode = F, EntityCodeTeacherNumber = F, SectionIDClonedTo = F, OffenseCount = F, OffenseCountYTD = F, HasCalculationGroupCourse = F, HasCategoriesInDistrict = F, HasGradeMarksInEntity = F, HasGradingPeriodGradeBuckets = F, HasGradingScales = F, HasValidGradebookSetup = F, HasValidStandardsSetup = F, InPastYear = F, IsSelfPacedAndActive = F, AssignmentCount = F, MissingAssignmentCount = F, ScoredAssignmentCount = F, UnscoredAssignmentCount = F, DisplayForTeachers = F, GradebookCanHaveSettingsCopiedFromPreviousYear = F, EffectiveTeacherFirstLastName = F, EffectiveTeacherLastFirstName = F, ViewingFromOfferingEntity = F, CanBeOffered = F, IsOffered = F, IsActiveOverride = F, HasAssignments = F, StudentCountForTerm = F, NonGradedAssignmentCountForTerm = F, NonGradedAssignmentCountNoStudentAssignmentsForTerm = F, DueDateOfLastAssignmentScored = F, ScoredAssignmentRange100to90CurrentTerm = F, ScoredAssignmentRange89to80CurrentTerm = F, ScoredAssignmentRange79to70CurrentTerm = F, ScoredAssignmentRange69to60CurrentTerm = F, ScoredAssignmentRange59to50CurrentTerm = F, ScoredAssignmentRange49to1CurrentTerm = F, ScoredAssignmentRange0CurrentTerm = F, StudentAssignmentDataString = F, AssignmentDataString = F, ExcusedAbsencesForTerm = F, ExcusedAbsencesYTD = F, UnexcusedAbsencesForTerm = F, UnexcusedAbsencesYTD = F, TardiesForTerm = F, TardiesYTD = F, OtherAbsencesForTerm = F, OtherAbsencesYTD = F, CurrentGradingPeriod = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MeetIDCurrentStoredPrimary = F, StaffMeetIDCurrentStoredPrimary = F, DisplayPeriodIDCurrentStoredPrimary = F, MeetSummaryIDCurrentStoredPrimary = F, StaffIDCurrentStoredPrimary = F, PreviousVersionOfFitCode = F, TotalSeatsOfferedToOtherEntities = F, StateSTARModeOfTeachingMNID = F, IsAdministeredForTSA = F, IsTSAProficient = F, SelfPacedEndTime = F, AssignmentsHaveBeenCreated = F, HasPreviousYearSettings = F, EntityID = F, SchoolYearID = F, HasAssignmentsWithSubjects = F, HasAssignmentsWithAcademicStandards = F, HasSubjectsCheckSectionLengthGradingPeriodSet = F, HasStandardsCheckSectionLengthGradingPeriodSet = F, SingleSex = F, IsCreditRecovery = F, IsOfferedToAnotherEntity = F, LockSectionFromSectionScheduler = F, LockSectionLengthFromSectionScheduler = F, SectionLengthScheduledBySectionScheduler = F, CurrentlyRecalculating = F, HasValidSubjectsSetup = F, TransferCourseEnrollmentCountFemales = F, TransferCourseEnrollmentCountMales = F, NonTransferCourseEnrollmentCountFemales = F, NonTransferCourseEnrollmentCountMales = F, TransferCourseEnrollmentCountFemalesEntity = F, TransferCourseEnrollmentCountMalesEntity = F, NonTransferCourseEnrollmentCountFemalesEntity = F, NonTransferCourseEnrollmentCountMalesEntity = F, TransferCourseEnrollmentCountFemalesToday = F, TransferCourseEnrollmentCountMalesToday = F, NonTransferCourseEnrollmentCountFemalesToday = F, NonTransferCourseEnrollmentCountMalesToday = F, TransferCourseEnrollmentCountFemalesEntityToday = F, TransferCourseEnrollmentCountMalesEntityToday = F, NonTransferCourseEnrollmentCountFemalesEntityToday = F, NonTransferCourseEnrollmentCountMalesEntityToday = F, TransferCourseEnrollmentCountFemalesSpecifiedDate = F, TransferCourseEnrollmentCountMalesSpecifiedDate = F, NonTransferCourseEnrollmentCountFemalesSpecifiedDate = F, NonTransferCourseEnrollmentCountMalesSpecifiedDate = F, TransferCourseEnrollmentCountFemalesEntitySpecifiedDate = F, TransferCourseEnrollmentCountMalesEntitySpecifiedDate = F, NonTransferCourseEnrollmentCountFemalesEntitySpecifiedDate = F, NonTransferCourseEnrollmentCountMalesEntitySpecifiedDate = F, TransferCourseEnrollmentCountFemalesFirstDay = F, TransferCourseEnrollmentCountMalesFirstDay = F, NonTransferCourseEnrollmentCountFemalesFirstDay = F, NonTransferCourseEnrollmentCountMalesFirstDay = F, TransferCourseEnrollmentCountFemalesEntityFirstDay = F, TransferCourseEnrollmentCountMalesEntityFirstDay = F, NonTransferCourseEnrollmentCountFemalesEntityFirstDay = F, NonTransferCourseEnrollmentCountMalesEntityFirstDay = F, ProgressStatusTodayCode = F, ProgressStatusSpecifiedDateCode = F, MaximumStudentCountOfferedToSpecifiedEntity = F, TotalEnrollmentCountForFilter = F, TotalEnrollmentCountEntityForFilter = F, CurrentEnrollmentForFilter = F, CurrentEnrollmentEntityForFilter = F, NumberOfTransferStudentSectionsForFilter = F, StudentEnrollmentForFilter = F, StudentEnrollmentEntityForFilter = F, StudentEnrollmentAsOfSpecifiedDateForFilter = F, StudentEnrollmentFemalesForFilter = F, StudentEnrollmentFemalesEntityForFilter = F, StudentEnrollmentFemalesAsOfSpecifiedDateForFilter = F, StudentEnrollmentMalesForFilter = F, StudentEnrollmentMalesEntityForFilter = F, StudentEnrollmentMalesAsOfSpecifiedDateForFilter = F, SeatsAvailableForFilter = F, SeatsAvailableEntityForFilter = F, MaximumStudentCountOfferedForFilter = F, IsInProgressForFilter = F, HasNotStartedForFilter = F, HasCompletedForFilter = F, MaximumStudentCountEntityForFilter = F, ExcludeFromStudentSectionLink = F, HasStudentSections = F, NonTransferCourseStudentEnrollmentCount = F, NonTransferCourseStudentEnrollmentCountEntity = F, SpecifiedPeriodDaySummary = F, ThisPeriodDaySummary = F, SectionEnrollmentTotalsForSectionLengthSubsetSummary = F, SectionEnrollmentTotalsForSectionLengthSubsetSummaryForEntity = F, HasAssignmentsWithStudentGroups = F, HasIncompleteClosedGradeChangeRequests = F, CourseCodeSectionCodeCourseDescription = F, BellScheduleGroupID = F, CalculatedBellScheduleGroupID = F, HasNonDeletedAssignments = F, RoomSeatsAvailable = F, UsingCurriculumSubjectsInGradebook = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "Section", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get Section
	#'
	#' This function returns a dataframe or json object of a Section
	#' @param SectionID The ID of the Section to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Section') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSection <- function(SectionID, SectionMNID = F, LanguageID = F, StateInstructionalMethodCodeMNID = F, CalendarIDMCCCOverride = F, CourseID = F, EdFiEducationalEnvironmentID = F, AllowCECE = F, Code = F, IsActive = F, SectionLengthID = F, SpecialEdPercentageLimit = F, IsBilingual = F, Website = F, MinimumStudentCount = F, MaximumStudentCount = F, OptimalStudentCount = F, ReservedSeatCount = F, RecalculateGradebook = F, RecalculateGradebookAdmin = F, AllowStudentsWithoutCategoryToBeAssigned = F, SectionIDClonedFrom = F, HomeroomID = F, FitCode = F, IsSelfPaced = F, IsAHistoricRecord = F, TotalMeetCount = F, CanAddStudentSection = F, SchedulingCategories = F, SchedulingTeams = F, CourseCodeSectionCode = F, CourseDescriptionCodeSectionCode = F, EntityCodeTeacherNumber = F, SectionIDClonedTo = F, OffenseCount = F, OffenseCountYTD = F, HasCalculationGroupCourse = F, HasCategoriesInDistrict = F, HasGradeMarksInEntity = F, HasGradingPeriodGradeBuckets = F, HasGradingScales = F, HasValidGradebookSetup = F, HasValidStandardsSetup = F, InPastYear = F, IsSelfPacedAndActive = F, AssignmentCount = F, MissingAssignmentCount = F, ScoredAssignmentCount = F, UnscoredAssignmentCount = F, DisplayForTeachers = F, GradebookCanHaveSettingsCopiedFromPreviousYear = F, EffectiveTeacherFirstLastName = F, EffectiveTeacherLastFirstName = F, ViewingFromOfferingEntity = F, CanBeOffered = F, IsOffered = F, IsActiveOverride = F, HasAssignments = F, StudentCountForTerm = F, NonGradedAssignmentCountForTerm = F, NonGradedAssignmentCountNoStudentAssignmentsForTerm = F, DueDateOfLastAssignmentScored = F, ScoredAssignmentRange100to90CurrentTerm = F, ScoredAssignmentRange89to80CurrentTerm = F, ScoredAssignmentRange79to70CurrentTerm = F, ScoredAssignmentRange69to60CurrentTerm = F, ScoredAssignmentRange59to50CurrentTerm = F, ScoredAssignmentRange49to1CurrentTerm = F, ScoredAssignmentRange0CurrentTerm = F, StudentAssignmentDataString = F, AssignmentDataString = F, ExcusedAbsencesForTerm = F, ExcusedAbsencesYTD = F, UnexcusedAbsencesForTerm = F, UnexcusedAbsencesYTD = F, TardiesForTerm = F, TardiesYTD = F, OtherAbsencesForTerm = F, OtherAbsencesYTD = F, CurrentGradingPeriod = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MeetIDCurrentStoredPrimary = F, StaffMeetIDCurrentStoredPrimary = F, DisplayPeriodIDCurrentStoredPrimary = F, MeetSummaryIDCurrentStoredPrimary = F, StaffIDCurrentStoredPrimary = F, PreviousVersionOfFitCode = F, TotalSeatsOfferedToOtherEntities = F, StateSTARModeOfTeachingMNID = F, IsAdministeredForTSA = F, IsTSAProficient = F, SelfPacedEndTime = F, AssignmentsHaveBeenCreated = F, HasPreviousYearSettings = F, EntityID = F, SchoolYearID = F, HasAssignmentsWithSubjects = F, HasAssignmentsWithAcademicStandards = F, HasSubjectsCheckSectionLengthGradingPeriodSet = F, HasStandardsCheckSectionLengthGradingPeriodSet = F, SingleSex = F, IsCreditRecovery = F, IsOfferedToAnotherEntity = F, LockSectionFromSectionScheduler = F, LockSectionLengthFromSectionScheduler = F, SectionLengthScheduledBySectionScheduler = F, CurrentlyRecalculating = F, HasValidSubjectsSetup = F, TransferCourseEnrollmentCountFemales = F, TransferCourseEnrollmentCountMales = F, NonTransferCourseEnrollmentCountFemales = F, NonTransferCourseEnrollmentCountMales = F, TransferCourseEnrollmentCountFemalesEntity = F, TransferCourseEnrollmentCountMalesEntity = F, NonTransferCourseEnrollmentCountFemalesEntity = F, NonTransferCourseEnrollmentCountMalesEntity = F, TransferCourseEnrollmentCountFemalesToday = F, TransferCourseEnrollmentCountMalesToday = F, NonTransferCourseEnrollmentCountFemalesToday = F, NonTransferCourseEnrollmentCountMalesToday = F, TransferCourseEnrollmentCountFemalesEntityToday = F, TransferCourseEnrollmentCountMalesEntityToday = F, NonTransferCourseEnrollmentCountFemalesEntityToday = F, NonTransferCourseEnrollmentCountMalesEntityToday = F, TransferCourseEnrollmentCountFemalesSpecifiedDate = F, TransferCourseEnrollmentCountMalesSpecifiedDate = F, NonTransferCourseEnrollmentCountFemalesSpecifiedDate = F, NonTransferCourseEnrollmentCountMalesSpecifiedDate = F, TransferCourseEnrollmentCountFemalesEntitySpecifiedDate = F, TransferCourseEnrollmentCountMalesEntitySpecifiedDate = F, NonTransferCourseEnrollmentCountFemalesEntitySpecifiedDate = F, NonTransferCourseEnrollmentCountMalesEntitySpecifiedDate = F, TransferCourseEnrollmentCountFemalesFirstDay = F, TransferCourseEnrollmentCountMalesFirstDay = F, NonTransferCourseEnrollmentCountFemalesFirstDay = F, NonTransferCourseEnrollmentCountMalesFirstDay = F, TransferCourseEnrollmentCountFemalesEntityFirstDay = F, TransferCourseEnrollmentCountMalesEntityFirstDay = F, NonTransferCourseEnrollmentCountFemalesEntityFirstDay = F, NonTransferCourseEnrollmentCountMalesEntityFirstDay = F, ProgressStatusTodayCode = F, ProgressStatusSpecifiedDateCode = F, MaximumStudentCountOfferedToSpecifiedEntity = F, TotalEnrollmentCountForFilter = F, TotalEnrollmentCountEntityForFilter = F, CurrentEnrollmentForFilter = F, CurrentEnrollmentEntityForFilter = F, NumberOfTransferStudentSectionsForFilter = F, StudentEnrollmentForFilter = F, StudentEnrollmentEntityForFilter = F, StudentEnrollmentAsOfSpecifiedDateForFilter = F, StudentEnrollmentFemalesForFilter = F, StudentEnrollmentFemalesEntityForFilter = F, StudentEnrollmentFemalesAsOfSpecifiedDateForFilter = F, StudentEnrollmentMalesForFilter = F, StudentEnrollmentMalesEntityForFilter = F, StudentEnrollmentMalesAsOfSpecifiedDateForFilter = F, SeatsAvailableForFilter = F, SeatsAvailableEntityForFilter = F, MaximumStudentCountOfferedForFilter = F, IsInProgressForFilter = F, HasNotStartedForFilter = F, HasCompletedForFilter = F, MaximumStudentCountEntityForFilter = F, ExcludeFromStudentSectionLink = F, HasStudentSections = F, NonTransferCourseStudentEnrollmentCount = F, NonTransferCourseStudentEnrollmentCountEntity = F, SpecifiedPeriodDaySummary = F, ThisPeriodDaySummary = F, SectionEnrollmentTotalsForSectionLengthSubsetSummary = F, SectionEnrollmentTotalsForSectionLengthSubsetSummaryForEntity = F, HasAssignmentsWithStudentGroups = F, HasIncompleteClosedGradeChangeRequests = F, CourseCodeSectionCodeCourseDescription = F, BellScheduleGroupID = F, CalculatedBellScheduleGroupID = F, HasNonDeletedAssignments = F, RoomSeatsAvailable = F, UsingCurriculumSubjectsInGradebook = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "Section", objectId = SectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFailedSectionLengthSubsets
	#'
	#' This function returns a dataframe or json object of TempFailedSectionLengthSubsets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedSectionLengthSubset') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempFailedSectionLengthSubsets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFailedSectionLengthSubsets <- function(searchConditionsList = NULL, TempFailedSectionLengthSubsetID = F, TempSectionLengthSubsetID = F, Note = F, SectionLengthSubsetID = F, CourseLengthCode = F, SectionLengthID = F, SectionLengthCode = F, Code = F, Description = F, StartDate = F, EndDate = F, OriginalStartDate = F, OriginalEndDate = F, IsFullSectionLength = F, ObjectIsDirty = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseLengthID = F, CourseLengthCodeDescription = F, SectionLengthCodeDescription = F, CodeDescription = F, SectionLengthStartDate = F, SectionLengthEndDate = F, IsUpdated = F, ProcessAction = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedSectionLengthSubset", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempFailedSectionLengthSubset
	#'
	#' This function returns a dataframe or json object of a TempFailedSectionLengthSubset
	#' @param TempFailedSectionLengthSubsetID The ID of the TempFailedSectionLengthSubset to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedSectionLengthSubset') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempFailedSectionLengthSubset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFailedSectionLengthSubset <- function(TempFailedSectionLengthSubsetID, TempSectionLengthSubsetID = F, Note = F, SectionLengthSubsetID = F, CourseLengthCode = F, SectionLengthID = F, SectionLengthCode = F, Code = F, Description = F, StartDate = F, EndDate = F, OriginalStartDate = F, OriginalEndDate = F, IsFullSectionLength = F, ObjectIsDirty = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseLengthID = F, CourseLengthCodeDescription = F, SectionLengthCodeDescription = F, CodeDescription = F, SectionLengthStartDate = F, SectionLengthEndDate = F, IsUpdated = F, ProcessAction = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFailedSectionLengthSubsetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempFailedSectionLengthSubset", objectId = TempFailedSectionLengthSubsetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchedulingGroupCourses
	#'
	#' This function returns a dataframe or json object of SchedulingGroupCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingGroupCourse') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SchedulingGroupCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchedulingGroupCourses <- function(searchConditionsList = NULL, SchedulingGroupCourseID = F, SchedulingGroupID = F, CourseID = F, SchedulingGroupCourseIDClonedFrom = F, SchedulingGroupCourseIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingGroupCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SchedulingGroupCourse
	#'
	#' This function returns a dataframe or json object of a SchedulingGroupCourse
	#' @param SchedulingGroupCourseID The ID of the SchedulingGroupCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingGroupCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSchedulingGroupCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchedulingGroupCourse <- function(SchedulingGroupCourseID, SchedulingGroupID = F, CourseID = F, SchedulingGroupCourseIDClonedFrom = F, SchedulingGroupCourseIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchedulingGroupCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SchedulingGroupCourse", objectId = SchedulingGroupCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ProcessRestrictions
	#'
	#' This function returns a dataframe or json object of ProcessRestrictions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ProcessRestriction') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of ProcessRestrictions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listProcessRestrictions <- function(searchConditionsList = NULL, ProcessRestrictionID = F, EntityID = F, SchoolYearID = F, UserIDLockCourseMasterPerformer = F, LockCourseMaster = F, LockMassStudentSchedulerUtility = F, LockMassUnscheduleStudentSectionsUtility = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LockCourseMasterSetByMassStudentSchedulerUtility = F, LockMassStudentSchedulerUtilitySetByMassStudentSchedulerUtility = F, UserIDLockMassStudentSchedulerUtilityPerformer = F, LockMassUnscheduleStudentSectionsUtilitySetByMassStudentSchedulerUtility = F, UserIDLockMassUnscheduleStudentSectionsUtilityPerformer = F, UserIDLockSchedulingBoardPerformer = F, LockSchedulingBoard = F, LockSchedulingBoardSetByMassStudentSchedulerUtility = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "ProcessRestriction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get ProcessRestriction
	#'
	#' This function returns a dataframe or json object of a ProcessRestriction
	#' @param ProcessRestrictionID The ID of the ProcessRestriction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ProcessRestriction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getProcessRestriction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getProcessRestriction <- function(ProcessRestrictionID, EntityID = F, SchoolYearID = F, UserIDLockCourseMasterPerformer = F, LockCourseMaster = F, LockMassStudentSchedulerUtility = F, LockMassUnscheduleStudentSectionsUtility = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LockCourseMasterSetByMassStudentSchedulerUtility = F, LockMassStudentSchedulerUtilitySetByMassStudentSchedulerUtility = F, UserIDLockMassStudentSchedulerUtilityPerformer = F, LockMassUnscheduleStudentSectionsUtilitySetByMassStudentSchedulerUtility = F, UserIDLockMassUnscheduleStudentSectionsUtilityPerformer = F, UserIDLockSchedulingBoardPerformer = F, LockSchedulingBoard = F, LockSchedulingBoardSetByMassStudentSchedulerUtility = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ProcessRestrictionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "ProcessRestriction", objectId = ProcessRestrictionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchedulingTeamGradeReferences
	#'
	#' This function returns a dataframe or json object of SchedulingTeamGradeReferences
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingTeamGradeReference') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SchedulingTeamGradeReferences
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchedulingTeamGradeReferences <- function(searchConditionsList = NULL, SchedulingTeamGradeReferenceID = F, SchedulingTeamID = F, GradeReferenceID = F, SchedulingTeamGradeReferenceIDClonedFrom = F, MaximumStudentCount = F, StudentEntityYearsCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingTeamGradeReference", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SchedulingTeamGradeReference
	#'
	#' This function returns a dataframe or json object of a SchedulingTeamGradeReference
	#' @param SchedulingTeamGradeReferenceID The ID of the SchedulingTeamGradeReference to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingTeamGradeReference') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSchedulingTeamGradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchedulingTeamGradeReference <- function(SchedulingTeamGradeReferenceID, SchedulingTeamID = F, GradeReferenceID = F, SchedulingTeamGradeReferenceIDClonedFrom = F, MaximumStudentCount = F, StudentEntityYearsCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchedulingTeamGradeReferenceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SchedulingTeamGradeReference", objectId = SchedulingTeamGradeReferenceID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFailedCourses
	#'
	#' This function returns a dataframe or json object of TempFailedCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedCourse') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempFailedCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFailedCourses <- function(searchConditionsList = NULL, TempFailedCourseID = F, TempCourseID = F, CourseID = F, CodeDescription = F, CourseCode = F, Description = F, IsActive = F, EarnedCredits = F, GradeLevelSummary = F, CourseSubjectCode = F, CourseTypeCode = F, ActiveSections = F, NumberOfAlternateCourseRequests = F, NumberOfCourseRequests = F, NumberOfSeatsAvailable = F, EstimatedNumberOfSections = F, MinimumSectionsRequired = F, AveragePerSectionMinimumSectionsRequired = F, OriginalEstimatedNumberOfSections = F, EstimatedStudentsPerSection = F, ObjectIsDirty = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NumberOfTransferStudentSections = F, CourseLengthID = F, CourseLengthCode = F, GradingPeriodSetID = F, GradingPeriodSetCode = F, NewCourseLengthCode = F, NewCourseLengthID = F, NewGradingPeriodSetCode = F, NewGradingPeriodSetID = F, DefaultSectionLengthID = F, RowIsSelected = F, RowIsReadOnly = F, Note = F, CurriculumCode = F, EntityCode = F, ObjectName = F, RecordsUpdated = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempFailedCourse
	#'
	#' This function returns a dataframe or json object of a TempFailedCourse
	#' @param TempFailedCourseID The ID of the TempFailedCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempFailedCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFailedCourse <- function(TempFailedCourseID, TempCourseID = F, CourseID = F, CodeDescription = F, CourseCode = F, Description = F, IsActive = F, EarnedCredits = F, GradeLevelSummary = F, CourseSubjectCode = F, CourseTypeCode = F, ActiveSections = F, NumberOfAlternateCourseRequests = F, NumberOfCourseRequests = F, NumberOfSeatsAvailable = F, EstimatedNumberOfSections = F, MinimumSectionsRequired = F, AveragePerSectionMinimumSectionsRequired = F, OriginalEstimatedNumberOfSections = F, EstimatedStudentsPerSection = F, ObjectIsDirty = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NumberOfTransferStudentSections = F, CourseLengthID = F, CourseLengthCode = F, GradingPeriodSetID = F, GradingPeriodSetCode = F, NewCourseLengthCode = F, NewCourseLengthID = F, NewGradingPeriodSetCode = F, NewGradingPeriodSetID = F, DefaultSectionLengthID = F, RowIsSelected = F, RowIsReadOnly = F, Note = F, CurriculumCode = F, EntityCode = F, ObjectName = F, RecordsUpdated = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFailedCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempFailedCourse", objectId = TempFailedCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchedulingGroups
	#'
	#' This function returns a dataframe or json object of SchedulingGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingGroup') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SchedulingGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchedulingGroups <- function(searchConditionsList = NULL, SchedulingGroupID = F, EntityID = F, SchoolYearID = F, Code = F, Description = F, GradeReferenceID = F, HomeroomID = F, SchedulingGroupIDClonedFrom = F, Type = F, CodeDescription = F, SchedulingGroupIDClonedTo = F, TotalSchedulingGroupCoursesCount = F, TotalSchedulingGroupSectionsCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SchedulingGroup
	#'
	#' This function returns a dataframe or json object of a SchedulingGroup
	#' @param SchedulingGroupID The ID of the SchedulingGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSchedulingGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchedulingGroup <- function(SchedulingGroupID, EntityID = F, SchoolYearID = F, Code = F, Description = F, GradeReferenceID = F, HomeroomID = F, SchedulingGroupIDClonedFrom = F, Type = F, CodeDescription = F, SchedulingGroupIDClonedTo = F, TotalSchedulingGroupCoursesCount = F, TotalSchedulingGroupSectionsCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchedulingGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SchedulingGroup", objectId = SchedulingGroupID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchedulingGroupSections
	#'
	#' This function returns a dataframe or json object of SchedulingGroupSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingGroupSection') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SchedulingGroupSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchedulingGroupSections <- function(searchConditionsList = NULL, SchedulingGroupSectionID = F, SchedulingGroupID = F, SectionID = F, SchedulingGroupSectionIDClonedFrom = F, SchedulingGroupSectionIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingGroupSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SchedulingGroupSection
	#'
	#' This function returns a dataframe or json object of a SchedulingGroupSection
	#' @param SchedulingGroupSectionID The ID of the SchedulingGroupSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingGroupSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSchedulingGroupSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchedulingGroupSection <- function(SchedulingGroupSectionID, SchedulingGroupID = F, SectionID = F, SchedulingGroupSectionIDClonedFrom = F, SchedulingGroupSectionIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchedulingGroupSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SchedulingGroupSection", objectId = SchedulingGroupSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCourses
	#'
	#' This function returns a dataframe or json object of TempCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourse') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCourses <- function(searchConditionsList = NULL, TempCourseID = F, CourseID = F, CodeDescription = F, CourseCode = F, Description = F, IsActive = F, EarnedCredits = F, GradeLevelSummary = F, CourseSubjectCode = F, CourseTypeCode = F, ActiveSections = F, NumberOfAlternateCourseRequests = F, NumberOfCourseRequests = F, NumberOfSeatsAvailable = F, EstimatedNumberOfSections = F, MinimumSectionsRequired = F, AveragePerSectionMinimumSectionsRequired = F, OriginalEstimatedNumberOfSections = F, EstimatedStudentsPerSection = F, ObjectIsDirty = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NumberOfTransferStudentSections = F, CourseLengthID = F, CourseLengthCode = F, GradingPeriodSetID = F, GradingPeriodSetCode = F, NewCourseLengthCode = F, NewCourseLengthID = F, NewGradingPeriodSetCode = F, NewGradingPeriodSetID = F, DefaultSectionLengthID = F, RowIsSelected = F, RowIsReadOnly = F, Note = F, CurriculumCode = F, EntityCode = F, ObjectName = F, RecordsUpdated = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempCourse
	#'
	#' This function returns a dataframe or json object of a TempCourse
	#' @param TempCourseID The ID of the TempCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCourse <- function(TempCourseID, CourseID = F, CodeDescription = F, CourseCode = F, Description = F, IsActive = F, EarnedCredits = F, GradeLevelSummary = F, CourseSubjectCode = F, CourseTypeCode = F, ActiveSections = F, NumberOfAlternateCourseRequests = F, NumberOfCourseRequests = F, NumberOfSeatsAvailable = F, EstimatedNumberOfSections = F, MinimumSectionsRequired = F, AveragePerSectionMinimumSectionsRequired = F, OriginalEstimatedNumberOfSections = F, EstimatedStudentsPerSection = F, ObjectIsDirty = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NumberOfTransferStudentSections = F, CourseLengthID = F, CourseLengthCode = F, GradingPeriodSetID = F, GradingPeriodSetCode = F, NewCourseLengthCode = F, NewCourseLengthID = F, NewGradingPeriodSetCode = F, NewGradingPeriodSetID = F, DefaultSectionLengthID = F, RowIsSelected = F, RowIsReadOnly = F, Note = F, CurriculumCode = F, EntityCode = F, ObjectName = F, RecordsUpdated = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempCourse", objectId = TempCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFailedSections
	#'
	#' This function returns a dataframe or json object of TempFailedSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedSection') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempFailedSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFailedSections <- function(searchConditionsList = NULL, TempFailedSectionID = F, TempSectionID = F, Note = F, CourseID = F, Course = F, SectionCode = F, SectionID = F, SectionLengthID = F, SectionLengthCode = F, SectionLengthStartDate = F, SectionLengthEndDate = F, CurrentEnrollment = F, MaximumStudentCount = F, PrimaryDisplayPeriod = F, PrimaryDays = F, StaffFullNameFML = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ErrorCount = F, HasErrors = F, TargetCourse = F, EntityIDCourse = F, SchoolYearIDCourse = F, CourseDescription = F, CourseIsActive = F, CourseTypeCode = F, IsActive = F, NumberOfTransferStudentSections = F, NewCourseLengthCode = F, NewCourseLengthID = F, NewSectionLengthCode = F, NewSectionLengthID = F, RowIsSelected = F, RowIsReadOnly = F, PeriodDaySummary = F, CourseEntityOfferedToID = F, EntityIDOfferedTo = F, GradeLevelSummary = F, IsSourceSection = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempFailedSection
	#'
	#' This function returns a dataframe or json object of a TempFailedSection
	#' @param TempFailedSectionID The ID of the TempFailedSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempFailedSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFailedSection <- function(TempFailedSectionID, TempSectionID = F, Note = F, CourseID = F, Course = F, SectionCode = F, SectionID = F, SectionLengthID = F, SectionLengthCode = F, SectionLengthStartDate = F, SectionLengthEndDate = F, CurrentEnrollment = F, MaximumStudentCount = F, PrimaryDisplayPeriod = F, PrimaryDays = F, StaffFullNameFML = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ErrorCount = F, HasErrors = F, TargetCourse = F, EntityIDCourse = F, SchoolYearIDCourse = F, CourseDescription = F, CourseIsActive = F, CourseTypeCode = F, IsActive = F, NumberOfTransferStudentSections = F, NewCourseLengthCode = F, NewCourseLengthID = F, NewSectionLengthCode = F, NewSectionLengthID = F, RowIsSelected = F, RowIsReadOnly = F, PeriodDaySummary = F, CourseEntityOfferedToID = F, EntityIDOfferedTo = F, GradeLevelSummary = F, IsSourceSection = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFailedSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempFailedSection", objectId = TempFailedSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSchedulingTeamGradeReferences
	#'
	#' This function returns a dataframe or json object of TempSchedulingTeamGradeReferences
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSchedulingTeamGradeReference') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempSchedulingTeamGradeReferences
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSchedulingTeamGradeReferences <- function(searchConditionsList = NULL, TempSchedulingTeamGradeReferenceID = F, SchedulingTeamGradeReferenceID = F, SchedulingTeamID = F, Code = F, Description = F, MaximumStudentCount = F, CurrentStudentCount = F, TotalToBeAssignedCount = F, TotalToBeAssignedPercent = F, TotalStudents = F, SortOrder = F, OverrideTotalToBeAssignedCount = F, OverrideTotalToBeAssignedPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempSchedulingTeamGradeReference", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempSchedulingTeamGradeReference
	#'
	#' This function returns a dataframe or json object of a TempSchedulingTeamGradeReference
	#' @param TempSchedulingTeamGradeReferenceID The ID of the TempSchedulingTeamGradeReference to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSchedulingTeamGradeReference') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempSchedulingTeamGradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSchedulingTeamGradeReference <- function(TempSchedulingTeamGradeReferenceID, SchedulingTeamGradeReferenceID = F, SchedulingTeamID = F, Code = F, Description = F, MaximumStudentCount = F, CurrentStudentCount = F, TotalToBeAssignedCount = F, TotalToBeAssignedPercent = F, TotalStudents = F, SortOrder = F, OverrideTotalToBeAssignedCount = F, OverrideTotalToBeAssignedPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSchedulingTeamGradeReferenceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempSchedulingTeamGradeReference", objectId = TempSchedulingTeamGradeReferenceID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentSchedulingCategories
	#'
	#' This function returns a dataframe or json object of TempStudentSchedulingCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentSchedulingCategory') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempStudentSchedulingCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentSchedulingCategories <- function(searchConditionsList = NULL, TempStudentSchedulingCategoryID = F, StudentNameLFM = F, SourceStudentEntityYearID = F, SourceCategoriesDisplayValue = F, SourceCategoryIDsCSV = F, TargetStudentEntityYearID = F, TargetCategoriesDisplayValue = F, TargetCategoryIDsCSV = F, ProposedCategoriesDisplayValue = F, ProposedTargetCategoryIDsToDeleteCSV = F, ProposedTargetCategoryIDsToInsertCSV = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentSchedulingCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempStudentSchedulingCategory
	#'
	#' This function returns a dataframe or json object of a TempStudentSchedulingCategory
	#' @param TempStudentSchedulingCategoryID The ID of the TempStudentSchedulingCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentSchedulingCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempStudentSchedulingCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentSchedulingCategory <- function(TempStudentSchedulingCategoryID, StudentNameLFM = F, SourceStudentEntityYearID = F, SourceCategoriesDisplayValue = F, SourceCategoryIDsCSV = F, TargetStudentEntityYearID = F, TargetCategoriesDisplayValue = F, TargetCategoryIDsCSV = F, ProposedCategoriesDisplayValue = F, ProposedTargetCategoryIDsToDeleteCSV = F, ProposedTargetCategoryIDsToInsertCSV = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentSchedulingCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempStudentSchedulingCategory", objectId = TempStudentSchedulingCategoryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentSchedulingTeams
	#'
	#' This function returns a dataframe or json object of TempStudentSchedulingTeams
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentSchedulingTeam') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempStudentSchedulingTeams
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentSchedulingTeams <- function(searchConditionsList = NULL, TempStudentSchedulingTeamID = F, StudentNameLFM = F, SourceStudentEntityYearID = F, SourceSchoolYearDescription = F, SourceSchedulingTeamID = F, SourceSchedulingTeamCode = F, SourceSchedulingTeamDescription = F, TargetStudentEntityYearID = F, TargetSchoolYearDescription = F, TargetSchedulingTeamID = F, TargetSchedulingTeamCode = F, TargetSchedulingTeamDescription = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentSchedulingTeam", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempStudentSchedulingTeam
	#'
	#' This function returns a dataframe or json object of a TempStudentSchedulingTeam
	#' @param TempStudentSchedulingTeamID The ID of the TempStudentSchedulingTeam to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentSchedulingTeam') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempStudentSchedulingTeam
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentSchedulingTeam <- function(TempStudentSchedulingTeamID, StudentNameLFM = F, SourceStudentEntityYearID = F, SourceSchoolYearDescription = F, SourceSchedulingTeamID = F, SourceSchedulingTeamCode = F, SourceSchedulingTeamDescription = F, TargetStudentEntityYearID = F, TargetSchoolYearDescription = F, TargetSchedulingTeamID = F, TargetSchedulingTeamCode = F, TargetSchedulingTeamDescription = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentSchedulingTeamID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempStudentSchedulingTeam", objectId = TempStudentSchedulingTeamID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAutoSchedulerStudentCourseRequestSectionDetails
	#'
	#' This function returns a dataframe or json object of StudentAutoSchedulerStudentCourseRequestSectionDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerStudentCourseRequestSectionDetail') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentAutoSchedulerStudentCourseRequestSectionDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAutoSchedulerStudentCourseRequestSectionDetails <- function(searchConditionsList = NULL, StudentAutoSchedulerStudentCourseRequestSectionDetailID = F, StudentAutoSchedulerStudentCourseRequestSectionID = F, SectionConflictReason = F, SectionConflictReasonText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerStudentCourseRequestSectionDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentAutoSchedulerStudentCourseRequestSectionDetail
	#'
	#' This function returns a dataframe or json object of a StudentAutoSchedulerStudentCourseRequestSectionDetail
	#' @param StudentAutoSchedulerStudentCourseRequestSectionDetailID The ID of the StudentAutoSchedulerStudentCourseRequestSectionDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerStudentCourseRequestSectionDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentAutoSchedulerStudentCourseRequestSectionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAutoSchedulerStudentCourseRequestSectionDetail <- function(StudentAutoSchedulerStudentCourseRequestSectionDetailID, StudentAutoSchedulerStudentCourseRequestSectionID = F, SectionConflictReason = F, SectionConflictReasonText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAutoSchedulerStudentCourseRequestSectionDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentAutoSchedulerStudentCourseRequestSectionDetail", objectId = StudentAutoSchedulerStudentCourseRequestSectionDetailID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAutoSchedulerStudentCourseRequestSections
	#'
	#' This function returns a dataframe or json object of StudentAutoSchedulerStudentCourseRequestSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerStudentCourseRequestSection') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentAutoSchedulerStudentCourseRequestSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAutoSchedulerStudentCourseRequestSections <- function(searchConditionsList = NULL, StudentAutoSchedulerStudentCourseRequestSectionID = F, StudentAutoSchedulerStudentCourseRequestID = F, StudentAutoSchedulerSectionID = F, SequenceNumber = F, AssignedToThisSection = F, EligibleToEnrollInSection = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SeatsRemaining = F, PeriodDaySummary = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerStudentCourseRequestSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentAutoSchedulerStudentCourseRequestSection
	#'
	#' This function returns a dataframe or json object of a StudentAutoSchedulerStudentCourseRequestSection
	#' @param StudentAutoSchedulerStudentCourseRequestSectionID The ID of the StudentAutoSchedulerStudentCourseRequestSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerStudentCourseRequestSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentAutoSchedulerStudentCourseRequestSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAutoSchedulerStudentCourseRequestSection <- function(StudentAutoSchedulerStudentCourseRequestSectionID, StudentAutoSchedulerStudentCourseRequestID = F, StudentAutoSchedulerSectionID = F, SequenceNumber = F, AssignedToThisSection = F, EligibleToEnrollInSection = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SeatsRemaining = F, PeriodDaySummary = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAutoSchedulerStudentCourseRequestSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentAutoSchedulerStudentCourseRequestSection", objectId = StudentAutoSchedulerStudentCourseRequestSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFailedStudentSectionTransactions
	#'
	#' This function returns a dataframe or json object of TempFailedStudentSectionTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedStudentSectionTransaction') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempFailedStudentSectionTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFailedStudentSectionTransactions <- function(searchConditionsList = NULL, TempFailedStudentSectionTransactionID = F, TempStudentSectionTransactionID = F, Note = F, StudentNameLFM = F, StudentNumber = F, Course = F, SectionCode = F, StudentSectionTransactionID = F, StudentCourseRequestID = F, TempStudentCourseRequestToReactivateID = F, TempStudentCourseRequestID = F, SectionLengthSubsetID = F, StartDate = F, EndDate = F, EarlyExitReasonID = F, NameIDRequestedBy = F, EntityIDCountsAs = F, HideNewStudentButton = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedStudentSectionTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempFailedStudentSectionTransaction
	#'
	#' This function returns a dataframe or json object of a TempFailedStudentSectionTransaction
	#' @param TempFailedStudentSectionTransactionID The ID of the TempFailedStudentSectionTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedStudentSectionTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempFailedStudentSectionTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFailedStudentSectionTransaction <- function(TempFailedStudentSectionTransactionID, TempStudentSectionTransactionID = F, Note = F, StudentNameLFM = F, StudentNumber = F, Course = F, SectionCode = F, StudentSectionTransactionID = F, StudentCourseRequestID = F, TempStudentCourseRequestToReactivateID = F, TempStudentCourseRequestID = F, SectionLengthSubsetID = F, StartDate = F, EndDate = F, EarlyExitReasonID = F, NameIDRequestedBy = F, EntityIDCountsAs = F, HideNewStudentButton = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFailedStudentSectionTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempFailedStudentSectionTransaction", objectId = TempFailedStudentSectionTransactionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSections
	#'
	#' This function returns a dataframe or json object of TempSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSection') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSections <- function(searchConditionsList = NULL, TempSectionID = F, CourseID = F, Course = F, SectionCode = F, SectionID = F, SectionLengthID = F, SectionLengthCode = F, SectionLengthStartDate = F, SectionLengthEndDate = F, CurrentEnrollment = F, MaximumStudentCount = F, PrimaryDisplayPeriod = F, PrimaryDays = F, StaffFullNameFML = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TargetCourse = F, EntityIDCourse = F, SchoolYearIDCourse = F, CourseDescription = F, CourseIsActive = F, CourseTypeCode = F, IsActive = F, NumberOfTransferStudentSections = F, NewCourseLengthCode = F, NewCourseLengthID = F, NewSectionLengthCode = F, NewSectionLengthID = F, RowIsSelected = F, RowIsReadOnly = F, Note = F, PeriodDaySummary = F, CourseEntityOfferedToID = F, EntityIDOfferedTo = F, GradeLevelSummary = F, IsSourceSection = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempSection
	#'
	#' This function returns a dataframe or json object of a TempSection
	#' @param TempSectionID The ID of the TempSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSection <- function(TempSectionID, CourseID = F, Course = F, SectionCode = F, SectionID = F, SectionLengthID = F, SectionLengthCode = F, SectionLengthStartDate = F, SectionLengthEndDate = F, CurrentEnrollment = F, MaximumStudentCount = F, PrimaryDisplayPeriod = F, PrimaryDays = F, StaffFullNameFML = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TargetCourse = F, EntityIDCourse = F, SchoolYearIDCourse = F, CourseDescription = F, CourseIsActive = F, CourseTypeCode = F, IsActive = F, NumberOfTransferStudentSections = F, NewCourseLengthCode = F, NewCourseLengthID = F, NewSectionLengthCode = F, NewSectionLengthID = F, RowIsSelected = F, RowIsReadOnly = F, Note = F, PeriodDaySummary = F, CourseEntityOfferedToID = F, EntityIDOfferedTo = F, GradeLevelSummary = F, IsSourceSection = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempSection", objectId = TempSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentCourseRequestToReactivateNonStates
	#'
	#' This function returns a dataframe or json object of TempStudentCourseRequestToReactivateNonStates
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentCourseRequestToReactivateNonState') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempStudentCourseRequestToReactivateNonStates
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentCourseRequestToReactivateNonStates <- function(searchConditionsList = NULL, TempStudentCourseRequestToReactivateNonStateID = F, StudentID = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, CourseCode = F, CourseDescription = F, EarnedCredits = F, CourseGradeLevelSummary = F, SectionCode = F, CurrentEnrollment = F, MaximumStudentCount = F, Period = F, Days = F, StaffFullNameFML = F, StudentCourseRequestID = F, IsAlternate = F, AlternateRank = F, StudentCourseRequestIDAlternateFor = F, RequestStatus = F, RequestSource = F, SchedulingMethod = F, SectionID = F, SectionLengthID = F, ExcludeFromReportCardsAndTranscripts = F, IsTransferCourse = F, UseEarnedCreditOverride = F, EarnedCreditOverride = F, GradeReferenceID = F, TransferCourseName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DateFrom = F, DateTo = F, CourseCodeDescription = F, AuditRecordIsSchedulable = F, UseEarnedCreditTotalOverride = F, TotalEarnedCreditOverride = F, TotalFailedCreditOverride = F, ExcludeFromStudentSectionLink = F, EntityIDCourse = F, SchoolYearIDCourse = F, StartDate = F, EndDate = F, SectionLengthSubsetID = F, EarlyExitReasonID = F, EarlyExitReasonCodeDescription = F, NameIDRequestedBy = F, NameRequestedByLFM = F, EntityIDCountsAs = F, PreventReactivateCheckboxFromBeingRendered = F, AuditRecordIsRequestable = F, StudentSectionID = F, StudentSectionTransactionID = F, SectionLengthSubsetCode = F, SectionLengthSubsetDescription = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentCourseRequestToReactivateNonState", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempStudentCourseRequestToReactivateNonState
	#'
	#' This function returns a dataframe or json object of a TempStudentCourseRequestToReactivateNonState
	#' @param TempStudentCourseRequestToReactivateNonStateID The ID of the TempStudentCourseRequestToReactivateNonState to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentCourseRequestToReactivateNonState') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempStudentCourseRequestToReactivateNonState
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentCourseRequestToReactivateNonState <- function(TempStudentCourseRequestToReactivateNonStateID, StudentID = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, CourseCode = F, CourseDescription = F, EarnedCredits = F, CourseGradeLevelSummary = F, SectionCode = F, CurrentEnrollment = F, MaximumStudentCount = F, Period = F, Days = F, StaffFullNameFML = F, StudentCourseRequestID = F, IsAlternate = F, AlternateRank = F, StudentCourseRequestIDAlternateFor = F, RequestStatus = F, RequestSource = F, SchedulingMethod = F, SectionID = F, SectionLengthID = F, ExcludeFromReportCardsAndTranscripts = F, IsTransferCourse = F, UseEarnedCreditOverride = F, EarnedCreditOverride = F, GradeReferenceID = F, TransferCourseName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DateFrom = F, DateTo = F, CourseCodeDescription = F, AuditRecordIsSchedulable = F, UseEarnedCreditTotalOverride = F, TotalEarnedCreditOverride = F, TotalFailedCreditOverride = F, ExcludeFromStudentSectionLink = F, EntityIDCourse = F, SchoolYearIDCourse = F, StartDate = F, EndDate = F, SectionLengthSubsetID = F, EarlyExitReasonID = F, EarlyExitReasonCodeDescription = F, NameIDRequestedBy = F, NameRequestedByLFM = F, EntityIDCountsAs = F, PreventReactivateCheckboxFromBeingRendered = F, AuditRecordIsRequestable = F, StudentSectionID = F, StudentSectionTransactionID = F, SectionLengthSubsetCode = F, SectionLengthSubsetDescription = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentCourseRequestToReactivateNonStateID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempStudentCourseRequestToReactivateNonState", objectId = TempStudentCourseRequestToReactivateNonStateID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentCourseRequestSectionLengthSubsets
	#'
	#' This function returns a dataframe or json object of TempStudentCourseRequestSectionLengthSubsets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentCourseRequestSectionLengthSubset') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempStudentCourseRequestSectionLengthSubsets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentCourseRequestSectionLengthSubsets <- function(searchConditionsList = NULL, TempStudentCourseRequestSectionLengthSubsetID = F, StudentCourseRequestSectionLengthSubsetID = F, StudentCourseRequestID = F, SectionLengthSubsetID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentCourseRequestSectionLengthSubset", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempStudentCourseRequestSectionLengthSubset
	#'
	#' This function returns a dataframe or json object of a TempStudentCourseRequestSectionLengthSubset
	#' @param TempStudentCourseRequestSectionLengthSubsetID The ID of the TempStudentCourseRequestSectionLengthSubset to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentCourseRequestSectionLengthSubset') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempStudentCourseRequestSectionLengthSubset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentCourseRequestSectionLengthSubset <- function(TempStudentCourseRequestSectionLengthSubsetID, StudentCourseRequestSectionLengthSubsetID = F, StudentCourseRequestID = F, SectionLengthSubsetID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentCourseRequestSectionLengthSubsetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempStudentCourseRequestSectionLengthSubset", objectId = TempStudentCourseRequestSectionLengthSubsetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentSectionTransactions
	#'
	#' This function returns a dataframe or json object of TempStudentSectionTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentSectionTransaction') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempStudentSectionTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentSectionTransactions <- function(searchConditionsList = NULL, TempStudentSectionTransactionID = F, StudentSectionTransactionID = F, StudentCourseRequestID = F, TempStudentCourseRequestToReactivateID = F, TempStudentCourseRequestID = F, SectionLengthSubsetID = F, StartDate = F, EndDate = F, EarlyExitReasonID = F, NameIDRequestedBy = F, EntityIDCountsAs = F, HideNewStudentButton = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentSectionTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempStudentSectionTransaction
	#'
	#' This function returns a dataframe or json object of a TempStudentSectionTransaction
	#' @param TempStudentSectionTransactionID The ID of the TempStudentSectionTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentSectionTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempStudentSectionTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentSectionTransaction <- function(TempStudentSectionTransactionID, StudentSectionTransactionID = F, StudentCourseRequestID = F, TempStudentCourseRequestToReactivateID = F, TempStudentCourseRequestID = F, SectionLengthSubsetID = F, StartDate = F, EndDate = F, EarlyExitReasonID = F, NameIDRequestedBy = F, EntityIDCountsAs = F, HideNewStudentButton = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentSectionTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempStudentSectionTransaction", objectId = TempStudentSectionTransactionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OpenPeriodAnalysisStudents
	#'
	#' This function returns a dataframe or json object of OpenPeriodAnalysisStudents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('OpenPeriodAnalysisStudent') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of OpenPeriodAnalysisStudents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOpenPeriodAnalysisStudents <- function(searchConditionsList = NULL, OpenPeriodAnalysisStudentID = F, OpenPeriodAnalysisID = F, StudentID = F, FirstName = F, MiddleName = F, LastName = F, GradeLevelCode = F, StudentNumber = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "OpenPeriodAnalysisStudent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get OpenPeriodAnalysisStudent
	#'
	#' This function returns a dataframe or json object of an OpenPeriodAnalysisStudent
	#' @param OpenPeriodAnalysisStudentID The ID of the OpenPeriodAnalysisStudent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('OpenPeriodAnalysisStudent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getOpenPeriodAnalysisStudent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOpenPeriodAnalysisStudent <- function(OpenPeriodAnalysisStudentID, OpenPeriodAnalysisID = F, StudentID = F, FirstName = F, MiddleName = F, LastName = F, GradeLevelCode = F, StudentNumber = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OpenPeriodAnalysisStudentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "OpenPeriodAnalysisStudent", objectId = OpenPeriodAnalysisStudentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OpenPeriodAnalyses
	#'
	#' This function returns a dataframe or json object of OpenPeriodAnalyses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('OpenPeriodAnalysis') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of OpenPeriodAnalyses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOpenPeriodAnalyses <- function(searchConditionsList = NULL, OpenPeriodAnalysisID = F, SchoolYearID = F, EntityID = F, UserIDPerformer = F, Status = F, StartTime = F, EndTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "OpenPeriodAnalysis", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get OpenPeriodAnalysis
	#'
	#' This function returns a dataframe or json object of an OpenPeriodAnalysis
	#' @param OpenPeriodAnalysisID The ID of the OpenPeriodAnalysis to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('OpenPeriodAnalysis') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getOpenPeriodAnalysis
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOpenPeriodAnalysis <- function(OpenPeriodAnalysisID, SchoolYearID = F, EntityID = F, UserIDPerformer = F, Status = F, StartTime = F, EndTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OpenPeriodAnalysisID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "OpenPeriodAnalysis", objectId = OpenPeriodAnalysisID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BaseRunAnalyses
	#'
	#' This function returns a dataframe or json object of BaseRunAnalyses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('BaseRunAnalysis') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of BaseRunAnalyses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBaseRunAnalyses <- function(searchConditionsList = NULL, BaseRunAnalysisID = F, SchoolYearID = F, EntityID = F, RunType = F, UserIDPerformer = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "BaseRunAnalysis", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get BaseRunAnalysis
	#'
	#' This function returns a dataframe or json object of a BaseRunAnalysis
	#' @param BaseRunAnalysisID The ID of the BaseRunAnalysis to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('BaseRunAnalysis') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getBaseRunAnalysis
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBaseRunAnalysis <- function(BaseRunAnalysisID, SchoolYearID = F, EntityID = F, RunType = F, UserIDPerformer = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BaseRunAnalysisID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "BaseRunAnalysis", objectId = BaseRunAnalysisID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudyHallSchedulerRunAnalyses
	#'
	#' This function returns a dataframe or json object of StudyHallSchedulerRunAnalyses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudyHallSchedulerRunAnalysis') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudyHallSchedulerRunAnalyses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudyHallSchedulerRunAnalyses <- function(searchConditionsList = NULL, StudyHallSchedulerRunAnalysisID = F, BaseRunAnalysisID = F, StartTimeAnalysis = F, EndTimeAnalysis = F, StartTimeFinalize = F, EndTimeFinalize = F, RunInformation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudyHallSchedulerRunAnalysis", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudyHallSchedulerRunAnalysis
	#'
	#' This function returns a dataframe or json object of a StudyHallSchedulerRunAnalysis
	#' @param StudyHallSchedulerRunAnalysisID The ID of the StudyHallSchedulerRunAnalysis to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudyHallSchedulerRunAnalysis') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudyHallSchedulerRunAnalysis
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudyHallSchedulerRunAnalysis <- function(StudyHallSchedulerRunAnalysisID, BaseRunAnalysisID = F, StartTimeAnalysis = F, EndTimeAnalysis = F, StartTimeFinalize = F, EndTimeFinalize = F, RunInformation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudyHallSchedulerRunAnalysisID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudyHallSchedulerRunAnalysis", objectId = StudyHallSchedulerRunAnalysisID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAutoSchedulerRunAnalysisExceptions
	#'
	#' This function returns a dataframe or json object of StudentAutoSchedulerRunAnalysisExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerRunAnalysisException') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentAutoSchedulerRunAnalysisExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAutoSchedulerRunAnalysisExceptions <- function(searchConditionsList = NULL, StudentAutoSchedulerRunAnalysisExceptionID = F, StudentAutoSchedulerRunAnalysisID = F, StudentAutoSchedulerStudentCourseRequestID = F, SeverityType = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseCode = F, CourseDescription = F, SectionCode = F, StudentFullName = F, StudentNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerRunAnalysisException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentAutoSchedulerRunAnalysisException
	#'
	#' This function returns a dataframe or json object of a StudentAutoSchedulerRunAnalysisException
	#' @param StudentAutoSchedulerRunAnalysisExceptionID The ID of the StudentAutoSchedulerRunAnalysisException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerRunAnalysisException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentAutoSchedulerRunAnalysisException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAutoSchedulerRunAnalysisException <- function(StudentAutoSchedulerRunAnalysisExceptionID, StudentAutoSchedulerRunAnalysisID = F, StudentAutoSchedulerStudentCourseRequestID = F, SeverityType = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseCode = F, CourseDescription = F, SectionCode = F, StudentFullName = F, StudentNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAutoSchedulerRunAnalysisExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentAutoSchedulerRunAnalysisException", objectId = StudentAutoSchedulerRunAnalysisExceptionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAutoSchedulerProposedStudentSectionTransactions
	#'
	#' This function returns a dataframe or json object of StudentAutoSchedulerProposedStudentSectionTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerProposedStudentSectionTransaction') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentAutoSchedulerProposedStudentSectionTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAutoSchedulerProposedStudentSectionTransactions <- function(searchConditionsList = NULL, StudentAutoSchedulerProposedStudentSectionTransactionID = F, BaseRunAnalysisID = F, CachedStudentSectionID = F, StudentSectionTransactionIDOriginal = F, StudentSectionTransactionID = F, StudentSectionIDOriginal = F, StudentSectionID = F, SectionIDOriginal = F, SectionID = F, SectionLengthSubsetIDOriginal = F, SectionLengthSubsetID = F, EntityIDCountsAs = F, StartDateOriginal = F, StartDate = F, EndDateOriginal = F, EndDate = F, DataCommittedToRealObjects = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentIDOriginal = F, StudentID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerProposedStudentSectionTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentAutoSchedulerProposedStudentSectionTransaction
	#'
	#' This function returns a dataframe or json object of a StudentAutoSchedulerProposedStudentSectionTransaction
	#' @param StudentAutoSchedulerProposedStudentSectionTransactionID The ID of the StudentAutoSchedulerProposedStudentSectionTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerProposedStudentSectionTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentAutoSchedulerProposedStudentSectionTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAutoSchedulerProposedStudentSectionTransaction <- function(StudentAutoSchedulerProposedStudentSectionTransactionID, BaseRunAnalysisID = F, CachedStudentSectionID = F, StudentSectionTransactionIDOriginal = F, StudentSectionTransactionID = F, StudentSectionIDOriginal = F, StudentSectionID = F, SectionIDOriginal = F, SectionID = F, SectionLengthSubsetIDOriginal = F, SectionLengthSubsetID = F, EntityIDCountsAs = F, StartDateOriginal = F, StartDate = F, EndDateOriginal = F, EndDate = F, DataCommittedToRealObjects = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentIDOriginal = F, StudentID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAutoSchedulerProposedStudentSectionTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentAutoSchedulerProposedStudentSectionTransaction", objectId = StudentAutoSchedulerProposedStudentSectionTransactionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAutoSchedulerProposedStudentSections
	#'
	#' This function returns a dataframe or json object of StudentAutoSchedulerProposedStudentSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerProposedStudentSection') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentAutoSchedulerProposedStudentSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAutoSchedulerProposedStudentSections <- function(searchConditionsList = NULL, StudentAutoSchedulerProposedStudentSectionID = F, BaseRunAnalysisID = F, CachedStudentSectionID = F, StudentSectionIDOriginal = F, StudentSectionID = F, StudentIDOriginal = F, StudentID = F, SectionIDOriginal = F, SectionID = F, GradeReferenceIDOriginal = F, GradeReferenceID = F, SequenceWithinEntireProcess = F, SequenceWithinGrade = F, SequenceWithinStudent = F, DataCommittedToRealObjects = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerProposedStudentSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentAutoSchedulerProposedStudentSection
	#'
	#' This function returns a dataframe or json object of a StudentAutoSchedulerProposedStudentSection
	#' @param StudentAutoSchedulerProposedStudentSectionID The ID of the StudentAutoSchedulerProposedStudentSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerProposedStudentSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentAutoSchedulerProposedStudentSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAutoSchedulerProposedStudentSection <- function(StudentAutoSchedulerProposedStudentSectionID, BaseRunAnalysisID = F, CachedStudentSectionID = F, StudentSectionIDOriginal = F, StudentSectionID = F, StudentIDOriginal = F, StudentID = F, SectionIDOriginal = F, SectionID = F, GradeReferenceIDOriginal = F, GradeReferenceID = F, SequenceWithinEntireProcess = F, SequenceWithinGrade = F, SequenceWithinStudent = F, DataCommittedToRealObjects = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAutoSchedulerProposedStudentSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentAutoSchedulerProposedStudentSection", objectId = StudentAutoSchedulerProposedStudentSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAutoSchedulerProposedStudentCourseRequests
	#'
	#' This function returns a dataframe or json object of StudentAutoSchedulerProposedStudentCourseRequests
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerProposedStudentCourseRequest') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentAutoSchedulerProposedStudentCourseRequests
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAutoSchedulerProposedStudentCourseRequests <- function(searchConditionsList = NULL, StudentAutoSchedulerProposedStudentCourseRequestID = F, BaseRunAnalysisID = F, CachedStudentSectionID = F, StudentCourseRequestIDOriginal = F, StudentCourseRequestID = F, StudentIDOriginal = F, StudentID = F, CourseIDOriginal = F, CourseID = F, EntityIDRequestedFrom = F, SectionIDOriginal = F, SectionID = F, StudentSectionIDOriginal = F, StudentSectionID = F, RequestStatusOriginal = F, CourseConflictReason = F, RequestStatus = F, SchedulingMethodOriginal = F, SchedulingMethod = F, SequenceWithinEntireProcess = F, SequenceWithinGrade = F, SequenceWithinStudent = F, IsAlternate = F, CourseConflictReasonText = F, SectionLengthSubsetSummary = F, Ignore = F, UnscheduleStatus = F, DataCommittedToRealObjects = F, IsNewlyScheduled = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionLengthSubsetsRequested = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerProposedStudentCourseRequest", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentAutoSchedulerProposedStudentCourseRequest
	#'
	#' This function returns a dataframe or json object of a StudentAutoSchedulerProposedStudentCourseRequest
	#' @param StudentAutoSchedulerProposedStudentCourseRequestID The ID of the StudentAutoSchedulerProposedStudentCourseRequest to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerProposedStudentCourseRequest') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentAutoSchedulerProposedStudentCourseRequest
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAutoSchedulerProposedStudentCourseRequest <- function(StudentAutoSchedulerProposedStudentCourseRequestID, BaseRunAnalysisID = F, CachedStudentSectionID = F, StudentCourseRequestIDOriginal = F, StudentCourseRequestID = F, StudentIDOriginal = F, StudentID = F, CourseIDOriginal = F, CourseID = F, EntityIDRequestedFrom = F, SectionIDOriginal = F, SectionID = F, StudentSectionIDOriginal = F, StudentSectionID = F, RequestStatusOriginal = F, CourseConflictReason = F, RequestStatus = F, SchedulingMethodOriginal = F, SchedulingMethod = F, SequenceWithinEntireProcess = F, SequenceWithinGrade = F, SequenceWithinStudent = F, IsAlternate = F, CourseConflictReasonText = F, SectionLengthSubsetSummary = F, Ignore = F, UnscheduleStatus = F, DataCommittedToRealObjects = F, IsNewlyScheduled = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionLengthSubsetsRequested = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAutoSchedulerProposedStudentCourseRequestID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentAutoSchedulerProposedStudentCourseRequest", objectId = StudentAutoSchedulerProposedStudentCourseRequestID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAutoSchedulerSections
	#'
	#' This function returns a dataframe or json object of StudentAutoSchedulerSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerSection') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentAutoSchedulerSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAutoSchedulerSections <- function(searchConditionsList = NULL, StudentAutoSchedulerSectionID = F, StudentAutoSchedulerCourseID = F, SectionID = F, TotalScheduled = F, SectionCode = F, SectionLengthCode = F, DisplayPeriodCode = F, DayRotationCode = F, IsActive = F, MinimumStudentCount = F, MaximumStudentCount = F, OptimalStudentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalNumberScheduledThisRun = F, AllowStudentsWithoutCategoryToBeAssigned = F, PrimaryMeetBuildingCode = F, PrimaryMeetRoomNumber = F, PrimaryMeetStaffNameLFM = F, SchedulingCategories = F, SchedulingTeams = F, PeriodDaySummary = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentAutoSchedulerSection
	#'
	#' This function returns a dataframe or json object of a StudentAutoSchedulerSection
	#' @param StudentAutoSchedulerSectionID The ID of the StudentAutoSchedulerSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentAutoSchedulerSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAutoSchedulerSection <- function(StudentAutoSchedulerSectionID, StudentAutoSchedulerCourseID = F, SectionID = F, TotalScheduled = F, SectionCode = F, SectionLengthCode = F, DisplayPeriodCode = F, DayRotationCode = F, IsActive = F, MinimumStudentCount = F, MaximumStudentCount = F, OptimalStudentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalNumberScheduledThisRun = F, AllowStudentsWithoutCategoryToBeAssigned = F, PrimaryMeetBuildingCode = F, PrimaryMeetRoomNumber = F, PrimaryMeetStaffNameLFM = F, SchedulingCategories = F, SchedulingTeams = F, PeriodDaySummary = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAutoSchedulerSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentAutoSchedulerSection", objectId = StudentAutoSchedulerSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAutoSchedulerStudentCourseRequests
	#'
	#' This function returns a dataframe or json object of StudentAutoSchedulerStudentCourseRequests
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerStudentCourseRequest') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentAutoSchedulerStudentCourseRequests
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAutoSchedulerStudentCourseRequests <- function(searchConditionsList = NULL, StudentAutoSchedulerStudentCourseRequestID = F, StudentAutoSchedulerStudentID = F, StudentAutoSchedulerCourseID = F, StudentAutoSchedulerSectionID = F, StudentCourseRequestID = F, EntityIDRequestedFrom = F, EntityRequestedFromEntityCode = F, CourseConflictReason = F, CourseConflictReasonText = F, SequenceNumber = F, SectionLengthSubsetSummary = F, SchedulingMethodCode = F, IsAlternate = F, HasRelatedStudentAutoSchedulerStudentCourseRequestSections = F, UpdatedDuringThisSchedulingRun = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionLengthSubsetsRequested = F, InitialSequenceNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerStudentCourseRequest", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentAutoSchedulerStudentCourseRequest
	#'
	#' This function returns a dataframe or json object of a StudentAutoSchedulerStudentCourseRequest
	#' @param StudentAutoSchedulerStudentCourseRequestID The ID of the StudentAutoSchedulerStudentCourseRequest to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerStudentCourseRequest') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentAutoSchedulerStudentCourseRequest
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAutoSchedulerStudentCourseRequest <- function(StudentAutoSchedulerStudentCourseRequestID, StudentAutoSchedulerStudentID = F, StudentAutoSchedulerCourseID = F, StudentAutoSchedulerSectionID = F, StudentCourseRequestID = F, EntityIDRequestedFrom = F, EntityRequestedFromEntityCode = F, CourseConflictReason = F, CourseConflictReasonText = F, SequenceNumber = F, SectionLengthSubsetSummary = F, SchedulingMethodCode = F, IsAlternate = F, HasRelatedStudentAutoSchedulerStudentCourseRequestSections = F, UpdatedDuringThisSchedulingRun = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionLengthSubsetsRequested = F, InitialSequenceNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAutoSchedulerStudentCourseRequestID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentAutoSchedulerStudentCourseRequest", objectId = StudentAutoSchedulerStudentCourseRequestID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAutoSchedulerStudents
	#'
	#' This function returns a dataframe or json object of StudentAutoSchedulerStudents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerStudent') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentAutoSchedulerStudents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAutoSchedulerStudents <- function(searchConditionsList = NULL, StudentAutoSchedulerStudentID = F, StudentAutoSchedulerRunAnalysisID = F, StudentID = F, SequenceNumber = F, HasConflict = F, FullName = F, Grade = F, BirthDate = F, StudentNumber = F, GenderCode = F, StudentTypeCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTimeAnalysis = F, EndTimeAnalysis = F, RawPermutations = F, SchedulesConsidered = F, NumberOfConflictedStudentCourseRequests = F, NumberOfScheduledStudentCourseRequests = F, TotalNumberOfStudentCourseRequests = F, GradeReferenceID = F, SchedulingCategories = F, SchedulingTeamCode = F, TotalNumberOfAlternateStudentCourseRequests = F, ProcessedDuringThisSchedulingRun = F, RandomSchedulingInteger = F, CalendarCode = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerStudent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentAutoSchedulerStudent
	#'
	#' This function returns a dataframe or json object of a StudentAutoSchedulerStudent
	#' @param StudentAutoSchedulerStudentID The ID of the StudentAutoSchedulerStudent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerStudent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentAutoSchedulerStudent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAutoSchedulerStudent <- function(StudentAutoSchedulerStudentID, StudentAutoSchedulerRunAnalysisID = F, StudentID = F, SequenceNumber = F, HasConflict = F, FullName = F, Grade = F, BirthDate = F, StudentNumber = F, GenderCode = F, StudentTypeCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTimeAnalysis = F, EndTimeAnalysis = F, RawPermutations = F, SchedulesConsidered = F, NumberOfConflictedStudentCourseRequests = F, NumberOfScheduledStudentCourseRequests = F, TotalNumberOfStudentCourseRequests = F, GradeReferenceID = F, SchedulingCategories = F, SchedulingTeamCode = F, TotalNumberOfAlternateStudentCourseRequests = F, ProcessedDuringThisSchedulingRun = F, RandomSchedulingInteger = F, CalendarCode = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAutoSchedulerStudentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentAutoSchedulerStudent", objectId = StudentAutoSchedulerStudentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAutoSchedulerRunAnalysisTotals
	#'
	#' This function returns a dataframe or json object of StudentAutoSchedulerRunAnalysisTotals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerRunAnalysisTotal') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentAutoSchedulerRunAnalysisTotals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAutoSchedulerRunAnalysisTotals <- function(searchConditionsList = NULL, StudentAutoSchedulerRunAnalysisTotalID = F, StudentAutoSchedulerRunAnalysisID = F, GradeReferenceID = F, TotalStudents = F, StudentsWithConflicts = F, ConflictPercent = F, TotalStudentCourseRequests = F, TotalConflictedStudentCourseRequests = F, TotalScheduledStudentCourseRequests = F, StudentCourseRequestsConflictedThisRun = F, StudentCourseRequestsProcessedThisRun = F, StudentCourseRequestsScheduledThisRun = F, ConflictedStudentCourseRequestPercent = F, ScheduledStudentCourseRequestPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTimeAnalysis = F, EndTimeAnalysis = F, StudentsWithOneConflict = F, StudentsWithTwoConflicts = F, StudentsWithThreeOrMoreConflicts = F, TotalAlternateStudentCourseRequests = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerRunAnalysisTotal", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentAutoSchedulerRunAnalysisTotal
	#'
	#' This function returns a dataframe or json object of a StudentAutoSchedulerRunAnalysisTotal
	#' @param StudentAutoSchedulerRunAnalysisTotalID The ID of the StudentAutoSchedulerRunAnalysisTotal to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerRunAnalysisTotal') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentAutoSchedulerRunAnalysisTotal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAutoSchedulerRunAnalysisTotal <- function(StudentAutoSchedulerRunAnalysisTotalID, StudentAutoSchedulerRunAnalysisID = F, GradeReferenceID = F, TotalStudents = F, StudentsWithConflicts = F, ConflictPercent = F, TotalStudentCourseRequests = F, TotalConflictedStudentCourseRequests = F, TotalScheduledStudentCourseRequests = F, StudentCourseRequestsConflictedThisRun = F, StudentCourseRequestsProcessedThisRun = F, StudentCourseRequestsScheduledThisRun = F, ConflictedStudentCourseRequestPercent = F, ScheduledStudentCourseRequestPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTimeAnalysis = F, EndTimeAnalysis = F, StudentsWithOneConflict = F, StudentsWithTwoConflicts = F, StudentsWithThreeOrMoreConflicts = F, TotalAlternateStudentCourseRequests = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAutoSchedulerRunAnalysisTotalID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentAutoSchedulerRunAnalysisTotal", objectId = StudentAutoSchedulerRunAnalysisTotalID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAutoSchedulerRunAnalyses
	#'
	#' This function returns a dataframe or json object of StudentAutoSchedulerRunAnalyses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerRunAnalysis') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentAutoSchedulerRunAnalyses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAutoSchedulerRunAnalyses <- function(searchConditionsList = NULL, StudentAutoSchedulerRunAnalysisID = F, BaseRunAnalysisID = F, StartTimeAnalysis = F, EndTimeAnalysis = F, StartTimeFinalize = F, EndTimeFinalize = F, Description = F, CloseSectionsWhenFilled = F, CreatedCourseAndSectionAnalysisDetails = F, CreatedStudentAnalysisDetails = F, ProcessSpecialEdCourses = F, StudentInformation = F, RunInformation = F, StudentCourseRequestOrder = F, StudentDifficultyOrder = F, StudentAutoSchedulerMode = F, AnalysisDuration = F, CourseConflictPercent = F, FinalizeDuration = F, OverallDuration = F, ProposedSchedulesAccepted = F, StudentConflictPercent = F, ConflictedStudentCourseRequestPercent = F, ScheduledStudentCourseRequestPercent = F, TotalScheduledStudentCourseRequests = F, TotalConflictedStudentCourseRequests = F, TotalStudentCourseRequests = F, StudentCourseRequestsConflictedThisRun = F, StudentCourseRequestsProcessedThisRun = F, StudentCourseRequestsScheduledThisRun = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalAlternateStudentCourseRequests = F, TotalStudents = F, TotalStudentsWithConflicts = F, TotalStudentsWithOneConflict = F, TotalStudentsWithTwoConflicts = F, TotalStudentsWithThreeOrMoreConflicts = F, CountOfStudentAutoSchedulerCourseRecords = F, CountOfStudentAutoSchedulerRunAnalysisExceptionRecords = F, CountOfStudentAutoSchedulerStudentRecords = F, AnalysisVersion = F, IncludedAbilityToAcceptProposedSchedules = F, PersistentSchedulingRunDataIsNoLongerAcceptable = F, SuccessfulStudentSectionsCount = F, FailedStudentSectionsCount = F, FailedStudentCourseRequestsCount = F, FailedStudentSectionTransactionsCount = F, FailedScheduledStudentSectionsCount = F, TotalStudentSectionsToFinalize = F, TotalStudentCourseRequestsToFinalize = F, TotalStudentSectionTransactionsToFinalize = F, SendMessageOnComplete = F, ExistsAutoScheduledCourses = F, ExistsAutoScheduledStudents = F, RunDescription = F, StudentEntityYearRangeXMLFilter = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerRunAnalysis", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentAutoSchedulerRunAnalysis
	#'
	#' This function returns a dataframe or json object of a StudentAutoSchedulerRunAnalysis
	#' @param StudentAutoSchedulerRunAnalysisID The ID of the StudentAutoSchedulerRunAnalysis to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerRunAnalysis') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentAutoSchedulerRunAnalysis
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAutoSchedulerRunAnalysis <- function(StudentAutoSchedulerRunAnalysisID, BaseRunAnalysisID = F, StartTimeAnalysis = F, EndTimeAnalysis = F, StartTimeFinalize = F, EndTimeFinalize = F, Description = F, CloseSectionsWhenFilled = F, CreatedCourseAndSectionAnalysisDetails = F, CreatedStudentAnalysisDetails = F, ProcessSpecialEdCourses = F, StudentInformation = F, RunInformation = F, StudentCourseRequestOrder = F, StudentDifficultyOrder = F, StudentAutoSchedulerMode = F, AnalysisDuration = F, CourseConflictPercent = F, FinalizeDuration = F, OverallDuration = F, ProposedSchedulesAccepted = F, StudentConflictPercent = F, ConflictedStudentCourseRequestPercent = F, ScheduledStudentCourseRequestPercent = F, TotalScheduledStudentCourseRequests = F, TotalConflictedStudentCourseRequests = F, TotalStudentCourseRequests = F, StudentCourseRequestsConflictedThisRun = F, StudentCourseRequestsProcessedThisRun = F, StudentCourseRequestsScheduledThisRun = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalAlternateStudentCourseRequests = F, TotalStudents = F, TotalStudentsWithConflicts = F, TotalStudentsWithOneConflict = F, TotalStudentsWithTwoConflicts = F, TotalStudentsWithThreeOrMoreConflicts = F, CountOfStudentAutoSchedulerCourseRecords = F, CountOfStudentAutoSchedulerRunAnalysisExceptionRecords = F, CountOfStudentAutoSchedulerStudentRecords = F, AnalysisVersion = F, IncludedAbilityToAcceptProposedSchedules = F, PersistentSchedulingRunDataIsNoLongerAcceptable = F, SuccessfulStudentSectionsCount = F, FailedStudentSectionsCount = F, FailedStudentCourseRequestsCount = F, FailedStudentSectionTransactionsCount = F, FailedScheduledStudentSectionsCount = F, TotalStudentSectionsToFinalize = F, TotalStudentCourseRequestsToFinalize = F, TotalStudentSectionTransactionsToFinalize = F, SendMessageOnComplete = F, ExistsAutoScheduledCourses = F, ExistsAutoScheduledStudents = F, RunDescription = F, StudentEntityYearRangeXMLFilter = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAutoSchedulerRunAnalysisID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentAutoSchedulerRunAnalysis", objectId = StudentAutoSchedulerRunAnalysisID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAutoSchedulerCourses
	#'
	#' This function returns a dataframe or json object of StudentAutoSchedulerCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerCourse') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentAutoSchedulerCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAutoSchedulerCourses <- function(searchConditionsList = NULL, StudentAutoSchedulerCourseID = F, StudentAutoSchedulerRunAnalysisID = F, CourseID = F, TotalRequests = F, TotalScheduled = F, TotalConflicts = F, CourseCode = F, CourseDescription = F, CourseEntityID = F, CourseEntityCode = F, CourseLengthCode = F, CourseSubjectCode = F, SchedulingTypeCode = F, SchedulingPriorityCode = F, IsRequired = F, IsActive = F, SchedulingTeamModeCode = F, DepartmentCode = F, ActiveSections = F, ConflictedRequestPercent = F, ScheduledRequestPercent = F, TotalSections = F, TotalSeatsAvailable = F, TotalSeatsScheduled = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalNumberScheduledThisRun = F, TotalAlternateRequests = F, CategoryCode = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentAutoSchedulerCourse
	#'
	#' This function returns a dataframe or json object of a StudentAutoSchedulerCourse
	#' @param StudentAutoSchedulerCourseID The ID of the StudentAutoSchedulerCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentAutoSchedulerCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentAutoSchedulerCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAutoSchedulerCourse <- function(StudentAutoSchedulerCourseID, StudentAutoSchedulerRunAnalysisID = F, CourseID = F, TotalRequests = F, TotalScheduled = F, TotalConflicts = F, CourseCode = F, CourseDescription = F, CourseEntityID = F, CourseEntityCode = F, CourseLengthCode = F, CourseSubjectCode = F, SchedulingTypeCode = F, SchedulingPriorityCode = F, IsRequired = F, IsActive = F, SchedulingTeamModeCode = F, DepartmentCode = F, ActiveSections = F, ConflictedRequestPercent = F, ScheduledRequestPercent = F, TotalSections = F, TotalSeatsAvailable = F, TotalSeatsScheduled = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalNumberScheduledThisRun = F, TotalAlternateRequests = F, CategoryCode = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAutoSchedulerCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentAutoSchedulerCourse", objectId = StudentAutoSchedulerCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFailedStudentSections
	#'
	#' This function returns a dataframe or json object of TempFailedStudentSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedStudentSection') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempFailedStudentSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFailedStudentSections <- function(searchConditionsList = NULL, TempFailedStudentSectionID = F, TempStudentSectionID = F, StudentSectionID = F, StudentCourseRequestID = F, StudentNameLFM = F, StudentNumber = F, CourseCodeDescription = F, SectionCode = F, SectionID = F, StudentID = F, StartDate = F, EndDate = F, GradeReferenceID = F, StudentGradeLevelCode = F, StudentGradYear = F, StudentGenderCode = F, CourseGradeLevelSummary = F, EarlyExitReasonCodeDescription = F, Note = F, StudentSectionTransactionIDToUpdate = F, RenderCheckbox = F, WorkflowAction = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, EntityIDCourse = F, SchoolYearIDCourse = F, CourseEntityOfferedToID = F, EntityIDCountsAs = F, TempStudentID = F, CourseCode = F, CourseDescription = F, SectionLengthID = F, SectionLengthCode = F, SectionLengthStartDate = F, SectionLengthEndDate = F, SectionLengthSubsetID = F, RowIsSelected = F, RowIsReadOnly = F, AutomaticRequestSetting = F, AutomaticScheduleSetting = F, SectionCorequisiteGroupName = F, ScheduleAllSectionsInGroupOrNone = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedStudentSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempFailedStudentSection
	#'
	#' This function returns a dataframe or json object of a TempFailedStudentSection
	#' @param TempFailedStudentSectionID The ID of the TempFailedStudentSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedStudentSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempFailedStudentSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFailedStudentSection <- function(TempFailedStudentSectionID, TempStudentSectionID = F, StudentSectionID = F, StudentCourseRequestID = F, StudentNameLFM = F, StudentNumber = F, CourseCodeDescription = F, SectionCode = F, SectionID = F, StudentID = F, StartDate = F, EndDate = F, GradeReferenceID = F, StudentGradeLevelCode = F, StudentGradYear = F, StudentGenderCode = F, CourseGradeLevelSummary = F, EarlyExitReasonCodeDescription = F, Note = F, StudentSectionTransactionIDToUpdate = F, RenderCheckbox = F, WorkflowAction = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, EntityIDCourse = F, SchoolYearIDCourse = F, CourseEntityOfferedToID = F, EntityIDCountsAs = F, TempStudentID = F, CourseCode = F, CourseDescription = F, SectionLengthID = F, SectionLengthCode = F, SectionLengthStartDate = F, SectionLengthEndDate = F, SectionLengthSubsetID = F, RowIsSelected = F, RowIsReadOnly = F, AutomaticRequestSetting = F, AutomaticScheduleSetting = F, SectionCorequisiteGroupName = F, ScheduleAllSectionsInGroupOrNone = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFailedStudentSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempFailedStudentSection", objectId = TempFailedStudentSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentCourseRequestSectionLengthSubsets
	#'
	#' This function returns a dataframe or json object of StudentCourseRequestSectionLengthSubsets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentCourseRequestSectionLengthSubset') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentCourseRequestSectionLengthSubsets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentCourseRequestSectionLengthSubsets <- function(searchConditionsList = NULL, StudentCourseRequestSectionLengthSubsetID = F, StudentCourseRequestID = F, SectionLengthSubsetID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentCourseRequestSectionLengthSubset", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentCourseRequestSectionLengthSubset
	#'
	#' This function returns a dataframe or json object of a StudentCourseRequestSectionLengthSubset
	#' @param StudentCourseRequestSectionLengthSubsetID The ID of the StudentCourseRequestSectionLengthSubset to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentCourseRequestSectionLengthSubset') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentCourseRequestSectionLengthSubset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentCourseRequestSectionLengthSubset <- function(StudentCourseRequestSectionLengthSubsetID, StudentCourseRequestID = F, SectionLengthSubsetID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentCourseRequestSectionLengthSubsetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentCourseRequestSectionLengthSubset", objectId = StudentCourseRequestSectionLengthSubsetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFailedStudentCourseRequests
	#'
	#' This function returns a dataframe or json object of TempFailedStudentCourseRequests
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedStudentCourseRequest') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempFailedStudentCourseRequests
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFailedStudentCourseRequests <- function(searchConditionsList = NULL, TempFailedStudentCourseRequestID = F, TempStudentCourseRequestID = F, Note = F, StudentID = F, FullStudentNameLFM = F, StudentNumber = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, CourseCode = F, CourseDescription = F, EarnedCredits = F, CourseSubjectCodeDescription = F, CourseDepartmentCodeDescription = F, CourseNumericSchoolYear = F, CourseSchoolYearDescription = F, CourseGradeLevelSummary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentCourseRequestID = F, StudentCourseRequestSectionLengthSubsetID = F, SectionLengthSubsetID = F, SectionlengthSubsetCode = F, CourseLengthCode = F, TempStudentEnrollmentRecordID = F, Selected = F, Failed = F, ErrorMessage = F, WorkflowAction = F, StudentSectionID = F, TempStudentID = F, SectionCode = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedStudentCourseRequest", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempFailedStudentCourseRequest
	#'
	#' This function returns a dataframe or json object of a TempFailedStudentCourseRequest
	#' @param TempFailedStudentCourseRequestID The ID of the TempFailedStudentCourseRequest to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedStudentCourseRequest') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempFailedStudentCourseRequest
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFailedStudentCourseRequest <- function(TempFailedStudentCourseRequestID, TempStudentCourseRequestID = F, Note = F, StudentID = F, FullStudentNameLFM = F, StudentNumber = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, CourseCode = F, CourseDescription = F, EarnedCredits = F, CourseSubjectCodeDescription = F, CourseDepartmentCodeDescription = F, CourseNumericSchoolYear = F, CourseSchoolYearDescription = F, CourseGradeLevelSummary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentCourseRequestID = F, StudentCourseRequestSectionLengthSubsetID = F, SectionLengthSubsetID = F, SectionlengthSubsetCode = F, CourseLengthCode = F, TempStudentEnrollmentRecordID = F, Selected = F, Failed = F, ErrorMessage = F, WorkflowAction = F, StudentSectionID = F, TempStudentID = F, SectionCode = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFailedStudentCourseRequestID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempFailedStudentCourseRequest", objectId = TempFailedStudentCourseRequestID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentCourseRequests
	#'
	#' This function returns a dataframe or json object of TempStudentCourseRequests
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentCourseRequest') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempStudentCourseRequests
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentCourseRequests <- function(searchConditionsList = NULL, TempStudentCourseRequestID = F, StudentID = F, FullStudentNameLFM = F, StudentNumber = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, CourseCode = F, CourseDescription = F, EarnedCredits = F, CourseSubjectCodeDescription = F, CourseDepartmentCodeDescription = F, CourseNumericSchoolYear = F, CourseSchoolYearDescription = F, CourseGradeLevelSummary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentCourseRequestID = F, StudentCourseRequestSectionLengthSubsetID = F, SectionLengthSubsetID = F, SectionlengthSubsetCode = F, CourseLengthCode = F, TempStudentEnrollmentRecordID = F, Selected = F, Failed = F, ErrorMessage = F, WorkflowAction = F, StudentSectionID = F, TempStudentID = F, SectionCode = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentCourseRequest", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempStudentCourseRequest
	#'
	#' This function returns a dataframe or json object of a TempStudentCourseRequest
	#' @param TempStudentCourseRequestID The ID of the TempStudentCourseRequest to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentCourseRequest') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempStudentCourseRequest
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentCourseRequest <- function(TempStudentCourseRequestID, StudentID = F, FullStudentNameLFM = F, StudentNumber = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, CourseCode = F, CourseDescription = F, EarnedCredits = F, CourseSubjectCodeDescription = F, CourseDepartmentCodeDescription = F, CourseNumericSchoolYear = F, CourseSchoolYearDescription = F, CourseGradeLevelSummary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentCourseRequestID = F, StudentCourseRequestSectionLengthSubsetID = F, SectionLengthSubsetID = F, SectionlengthSubsetCode = F, CourseLengthCode = F, TempStudentEnrollmentRecordID = F, Selected = F, Failed = F, ErrorMessage = F, WorkflowAction = F, StudentSectionID = F, TempStudentID = F, SectionCode = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentCourseRequestID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempStudentCourseRequest", objectId = TempStudentCourseRequestID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSectionLengths
	#'
	#' This function returns a dataframe or json object of TempSectionLengths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionLength') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempSectionLengths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSectionLengths <- function(searchConditionsList = NULL, TempSectionLengthID = F, SectionLengthID = F, Code = F, StartDate = F, EndDate = F, OriginalStartDate = F, OriginalEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseLengthCode = F, IsUpdated = F, ProcessAction = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempSectionLength", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempSectionLength
	#'
	#' This function returns a dataframe or json object of a TempSectionLength
	#' @param TempSectionLengthID The ID of the TempSectionLength to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionLength') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempSectionLength
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSectionLength <- function(TempSectionLengthID, SectionLengthID = F, Code = F, StartDate = F, EndDate = F, OriginalStartDate = F, OriginalEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseLengthCode = F, IsUpdated = F, ProcessAction = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSectionLengthID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempSectionLength", objectId = TempSectionLengthID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseGradeReferences
	#'
	#' This function returns a dataframe or json object of CourseGradeReferences
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseGradeReference') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseGradeReferences
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseGradeReferences <- function(searchConditionsList = NULL, CourseGradeReferenceID = F, CourseID = F, GradeReferenceID = F, EntityGroupKey = F, CourseGradeReferenceIDClonedFrom = F, NumberOfAlternateCourseRequests = F, NumberOfCourseRequests = F, NumberOfCourseRequestsFemales = F, NumberOfCourseRequestsMales = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NumberOfAlternateCourseRequestsEntity = F, NumberOfCourseRequestsEntity = F, NumberOfCourseRequestsFemalesEntity = F, NumberOfCourseRequestsMalesEntity = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseGradeReference", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseGradeReference
	#'
	#' This function returns a dataframe or json object of a CourseGradeReference
	#' @param CourseGradeReferenceID The ID of the CourseGradeReference to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseGradeReference') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseGradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseGradeReference <- function(CourseGradeReferenceID, CourseID = F, GradeReferenceID = F, EntityGroupKey = F, CourseGradeReferenceIDClonedFrom = F, NumberOfAlternateCourseRequests = F, NumberOfCourseRequests = F, NumberOfCourseRequestsFemales = F, NumberOfCourseRequestsMales = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NumberOfAlternateCourseRequestsEntity = F, NumberOfCourseRequestsEntity = F, NumberOfCourseRequestsFemalesEntity = F, NumberOfCourseRequestsMalesEntity = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseGradeReferenceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseGradeReference", objectId = CourseGradeReferenceID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSectionLengthSubsets
	#'
	#' This function returns a dataframe or json object of TempSectionLengthSubsets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionLengthSubset') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempSectionLengthSubsets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSectionLengthSubsets <- function(searchConditionsList = NULL, TempSectionLengthSubsetID = F, SectionLengthSubsetID = F, CourseLengthCode = F, SectionLengthID = F, SectionLengthCode = F, Code = F, Description = F, StartDate = F, EndDate = F, OriginalStartDate = F, OriginalEndDate = F, IsFullSectionLength = F, ObjectIsDirty = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseLengthID = F, CourseLengthCodeDescription = F, SectionLengthCodeDescription = F, CodeDescription = F, SectionLengthStartDate = F, SectionLengthEndDate = F, IsUpdated = F, ProcessAction = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempSectionLengthSubset", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempSectionLengthSubset
	#'
	#' This function returns a dataframe or json object of a TempSectionLengthSubset
	#' @param TempSectionLengthSubsetID The ID of the TempSectionLengthSubset to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSectionLengthSubset') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempSectionLengthSubset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSectionLengthSubset <- function(TempSectionLengthSubsetID, SectionLengthSubsetID = F, CourseLengthCode = F, SectionLengthID = F, SectionLengthCode = F, Code = F, Description = F, StartDate = F, EndDate = F, OriginalStartDate = F, OriginalEndDate = F, IsFullSectionLength = F, ObjectIsDirty = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseLengthID = F, CourseLengthCodeDescription = F, SectionLengthCodeDescription = F, CodeDescription = F, SectionLengthStartDate = F, SectionLengthEndDate = F, IsUpdated = F, ProcessAction = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSectionLengthSubsetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempSectionLengthSubset", objectId = TempSectionLengthSubsetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchedulingTempStudentSections
	#'
	#' This function returns a dataframe or json object of SchedulingTempStudentSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingTempStudentSection') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SchedulingTempStudentSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchedulingTempStudentSections <- function(searchConditionsList = NULL, TempStudentSectionID = F, StudentSectionID = F, StudentCourseRequestID = F, StudentNameLFM = F, StudentNumber = F, CourseCodeDescription = F, SectionCode = F, SectionID = F, StudentID = F, StartDate = F, EndDate = F, GradeReferenceID = F, StudentGradeLevelCode = F, StudentGradYear = F, StudentGenderCode = F, CourseGradeLevelSummary = F, EarlyExitReasonCodeDescription = F, Note = F, StudentSectionTransactionIDToUpdate = F, RenderCheckbox = F, WorkflowAction = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, EntityIDCourse = F, SchoolYearIDCourse = F, CourseEntityOfferedToID = F, EntityIDCountsAs = F, TempStudentID = F, CourseCode = F, CourseDescription = F, SectionLengthID = F, SectionLengthCode = F, SectionLengthStartDate = F, SectionLengthEndDate = F, SectionLengthSubsetID = F, RowIsSelected = F, RowIsReadOnly = F, AutomaticRequestSetting = F, AutomaticScheduleSetting = F, SectionCorequisiteGroupName = F, ScheduleAllSectionsInGroupOrNone = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SchedulingTempStudentSection
	#'
	#' This function returns a dataframe or json object of a SchedulingTempStudentSection
	#' @param SchedulingTempStudentSectionID The ID of the SchedulingTempStudentSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingTempStudentSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSchedulingTempStudentSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchedulingTempStudentSection <- function(SchedulingTempStudentSectionID, TempStudentSectionID = F, StudentSectionID = F, StudentCourseRequestID = F, StudentNameLFM = F, StudentNumber = F, CourseCodeDescription = F, SectionCode = F, SectionID = F, StudentID = F, StartDate = F, EndDate = F, GradeReferenceID = F, StudentGradeLevelCode = F, StudentGradYear = F, StudentGenderCode = F, CourseGradeLevelSummary = F, EarlyExitReasonCodeDescription = F, Note = F, StudentSectionTransactionIDToUpdate = F, RenderCheckbox = F, WorkflowAction = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, EntityIDCourse = F, SchoolYearIDCourse = F, CourseEntityOfferedToID = F, EntityIDCountsAs = F, TempStudentID = F, CourseCode = F, CourseDescription = F, SectionLengthID = F, SectionLengthCode = F, SectionLengthStartDate = F, SectionLengthEndDate = F, SectionLengthSubsetID = F, RowIsSelected = F, RowIsReadOnly = F, AutomaticRequestSetting = F, AutomaticScheduleSetting = F, SectionCorequisiteGroupName = F, ScheduleAllSectionsInGroupOrNone = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchedulingTempStudentSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempStudentSection", objectId = SchedulingTempStudentSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EarlyExitReasons
	#'
	#' This function returns a dataframe or json object of EarlyExitReasons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('EarlyExitReason') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of EarlyExitReasons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEarlyExitReasons <- function(searchConditionsList = NULL, EarlyExitReasonID = F, EntityID = F, EntityGroupKey = F, SchoolYearID = F, Code = F, Description = F, EarlyExitReasonIDClonedFrom = F, CodeDescription = F, IsConsideredDrop = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "EarlyExitReason", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get EarlyExitReason
	#'
	#' This function returns a dataframe or json object of an EarlyExitReason
	#' @param EarlyExitReasonID The ID of the EarlyExitReason to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('EarlyExitReason') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getEarlyExitReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEarlyExitReason <- function(EarlyExitReasonID, EntityID = F, EntityGroupKey = F, SchoolYearID = F, Code = F, Description = F, EarlyExitReasonIDClonedFrom = F, CodeDescription = F, IsConsideredDrop = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EarlyExitReasonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "EarlyExitReason", objectId = EarlyExitReasonID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSchedulingTeams
	#'
	#' This function returns a dataframe or json object of SectionSchedulingTeams
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulingTeam') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionSchedulingTeams
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSchedulingTeams <- function(searchConditionsList = NULL, SectionSchedulingTeamID = F, SectionID = F, SchedulingTeamID = F, SectionSchedulingTeamIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulingTeam", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSchedulingTeam
	#'
	#' This function returns a dataframe or json object of a SectionSchedulingTeam
	#' @param SectionSchedulingTeamID The ID of the SectionSchedulingTeam to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulingTeam') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionSchedulingTeam
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSchedulingTeam <- function(SectionSchedulingTeamID, SectionID = F, SchedulingTeamID = F, SectionSchedulingTeamIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSchedulingTeamID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionSchedulingTeam", objectId = SectionSchedulingTeamID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DateRangePresets
	#'
	#' This function returns a dataframe or json object of DateRangePresets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DateRangePreset') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of DateRangePresets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDateRangePresets <- function(searchConditionsList = NULL, DateRangePresetID = F, EntityID = F, SchoolYearID = F, Code = F, Description = F, LowDate = F, HighDate = F, EntityGroupKey = F, DateRangePresetIDClonedFrom = F, CodeDescription = F, DateRangePresetIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "DateRangePreset", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DateRangePreset
	#'
	#' This function returns a dataframe or json object of a DateRangePreset
	#' @param DateRangePresetID The ID of the DateRangePreset to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DateRangePreset') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getDateRangePreset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDateRangePreset <- function(DateRangePresetID, EntityID = F, SchoolYearID = F, Code = F, Description = F, LowDate = F, HighDate = F, EntityGroupKey = F, DateRangePresetIDClonedFrom = F, CodeDescription = F, DateRangePresetIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DateRangePresetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "DateRangePreset", objectId = DateRangePresetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AvailabilityFilterCourseStudents
	#'
	#' This function returns a dataframe or json object of AvailabilityFilterCourseStudents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AvailabilityFilterCourseStudent') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of AvailabilityFilterCourseStudents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAvailabilityFilterCourseStudents <- function(searchConditionsList = NULL, AvailabilityFilterCourseStudentID = F, Description = F, AvailabilityCourseFilterID = F, AvailabilityStudentFilterID = F, AvailabilityFilterCourseStudentIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExcludeAvailabilityListInNightlyUpdateTask = F, ExcludeAvailabilityListBoolUpdatedManually = F, UseForCourseRequests = F, UseForArenaScheduling = F, CourseRequestStartDate = F, ArenaSchedulingStartDate = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AvailabilityFilterCourseStudent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get AvailabilityFilterCourseStudent
	#'
	#' This function returns a dataframe or json object of an AvailabilityFilterCourseStudent
	#' @param AvailabilityFilterCourseStudentID The ID of the AvailabilityFilterCourseStudent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AvailabilityFilterCourseStudent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getAvailabilityFilterCourseStudent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAvailabilityFilterCourseStudent <- function(AvailabilityFilterCourseStudentID, Description = F, AvailabilityCourseFilterID = F, AvailabilityStudentFilterID = F, AvailabilityFilterCourseStudentIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExcludeAvailabilityListInNightlyUpdateTask = F, ExcludeAvailabilityListBoolUpdatedManually = F, UseForCourseRequests = F, UseForArenaScheduling = F, CourseRequestStartDate = F, ArenaSchedulingStartDate = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AvailabilityFilterCourseStudentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "AvailabilityFilterCourseStudent", objectId = AvailabilityFilterCourseStudentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AvailabilityStudentFilters
	#'
	#' This function returns a dataframe or json object of AvailabilityStudentFilters
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AvailabilityStudentFilter') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of AvailabilityStudentFilters
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAvailabilityStudentFilters <- function(searchConditionsList = NULL, AvailabilityStudentFilterID = F, Code = F, Description = F, Filter = F, EntityID = F, SchoolYearID = F, SkywardID = F, GradeReferenceIDInclusionList = F, StudentTypeIDInclusionList = F, AvailabilityStudentFilterIDClonedFrom = F, CodeDescription = F, AvailabilityStudentFilterIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AvailabilityStudentFilter", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get AvailabilityStudentFilter
	#'
	#' This function returns a dataframe or json object of an AvailabilityStudentFilter
	#' @param AvailabilityStudentFilterID The ID of the AvailabilityStudentFilter to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AvailabilityStudentFilter') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getAvailabilityStudentFilter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAvailabilityStudentFilter <- function(AvailabilityStudentFilterID, Code = F, Description = F, Filter = F, EntityID = F, SchoolYearID = F, SkywardID = F, GradeReferenceIDInclusionList = F, StudentTypeIDInclusionList = F, AvailabilityStudentFilterIDClonedFrom = F, CodeDescription = F, AvailabilityStudentFilterIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AvailabilityStudentFilterID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "AvailabilityStudentFilter", objectId = AvailabilityStudentFilterID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AvailabilityCourseFilters
	#'
	#' This function returns a dataframe or json object of AvailabilityCourseFilters
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AvailabilityCourseFilter') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of AvailabilityCourseFilters
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAvailabilityCourseFilters <- function(searchConditionsList = NULL, AvailabilityCourseFilterID = F, Code = F, Description = F, Filter = F, EntityID = F, SchoolYearID = F, SkywardID = F, IncludeCoursesWithNoCourseType = F, CourseTypeIDInclusionList = F, IncludeCategoryLunch = F, IncludeCategoryRegular = F, IncludeCategoryStudyHall = F, IncludeElective = F, IncludeRequired = F, IncludeSchedulingTypeNormal = F, IncludeSchedulingTypeManuallyScheduled = F, IncludeSchedulingTypeSpecialEducation = F, IncludeOfferedCourses = F, AvailabilityCourseFilterIDClonedFrom = F, CodeDescription = F, AvailabilityCourseFilterIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncludeInactiveCourses = F, GradeLevelFilterType = F, GradeReferenceIDInclusionList = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AvailabilityCourseFilter", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get AvailabilityCourseFilter
	#'
	#' This function returns a dataframe or json object of an AvailabilityCourseFilter
	#' @param AvailabilityCourseFilterID The ID of the AvailabilityCourseFilter to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AvailabilityCourseFilter') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getAvailabilityCourseFilter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAvailabilityCourseFilter <- function(AvailabilityCourseFilterID, Code = F, Description = F, Filter = F, EntityID = F, SchoolYearID = F, SkywardID = F, IncludeCoursesWithNoCourseType = F, CourseTypeIDInclusionList = F, IncludeCategoryLunch = F, IncludeCategoryRegular = F, IncludeCategoryStudyHall = F, IncludeElective = F, IncludeRequired = F, IncludeSchedulingTypeNormal = F, IncludeSchedulingTypeManuallyScheduled = F, IncludeSchedulingTypeSpecialEducation = F, IncludeOfferedCourses = F, AvailabilityCourseFilterIDClonedFrom = F, CodeDescription = F, AvailabilityCourseFilterIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncludeInactiveCourses = F, GradeLevelFilterType = F, GradeReferenceIDInclusionList = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AvailabilityCourseFilterID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "AvailabilityCourseFilter", objectId = AvailabilityCourseFilterID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ScheduleRestorePoints
	#'
	#' This function returns a dataframe or json object of ScheduleRestorePoints
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ScheduleRestorePoint') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of ScheduleRestorePoints
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listScheduleRestorePoints <- function(searchConditionsList = NULL, ScheduleRestorePointID = F, Name = F, Description = F, EntityID = F, SchoolYearID = F, RestorePointDateTime = F, NameDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportQueueID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "ScheduleRestorePoint", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get ScheduleRestorePoint
	#'
	#' This function returns a dataframe or json object of a ScheduleRestorePoint
	#' @param ScheduleRestorePointID The ID of the ScheduleRestorePoint to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ScheduleRestorePoint') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getScheduleRestorePoint
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getScheduleRestorePoint <- function(ScheduleRestorePointID, Name = F, Description = F, EntityID = F, SchoolYearID = F, RestorePointDateTime = F, NameDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportQueueID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ScheduleRestorePointID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "ScheduleRestorePoint", objectId = ScheduleRestorePointID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseAlternates
	#'
	#' This function returns a dataframe or json object of CourseAlternates
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseAlternate') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseAlternates
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseAlternates <- function(searchConditionsList = NULL, CourseAlternateID = F, CourseIDPrimary = F, CourseIDAlternate = F, CourseAlternateIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseAlternate", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseAlternate
	#'
	#' This function returns a dataframe or json object of a CourseAlternate
	#' @param CourseAlternateID The ID of the CourseAlternate to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseAlternate') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseAlternate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseAlternate <- function(CourseAlternateID, CourseIDPrimary = F, CourseIDAlternate = F, CourseAlternateIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseAlternateID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseAlternate", objectId = CourseAlternateID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSchedulerCourseConstraints
	#'
	#' This function returns a dataframe or json object of SectionSchedulerCourseConstraints
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerCourseConstraint') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionSchedulerCourseConstraints
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSchedulerCourseConstraints <- function(searchConditionsList = NULL, SectionSchedulerCourseConstraintID = F, CourseID = F, CourseIDLinked = F, SectionIDCurrentCourseSelected = F, SectionIDLinkedCourseSelected = F, LinkedCourse = F, Rule = F, CurrentCourseSection = F, LinkedCourseSection = F, IsActive = F, ScheduleLinkedCourseFirst = F, CurrentCourseTopSectionCount = F, LinkedCourseTopSectionCount = F, SectionSchedulerCourseConstraintIDClonedFrom = F, SectionSchedulerCourseConstraintIDClonedTo = F, ScheduleFirst = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerCourseConstraint", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSchedulerCourseConstraint
	#'
	#' This function returns a dataframe or json object of a SectionSchedulerCourseConstraint
	#' @param SectionSchedulerCourseConstraintID The ID of the SectionSchedulerCourseConstraint to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerCourseConstraint') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionSchedulerCourseConstraint
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSchedulerCourseConstraint <- function(SectionSchedulerCourseConstraintID, CourseID = F, CourseIDLinked = F, SectionIDCurrentCourseSelected = F, SectionIDLinkedCourseSelected = F, LinkedCourse = F, Rule = F, CurrentCourseSection = F, LinkedCourseSection = F, IsActive = F, ScheduleLinkedCourseFirst = F, CurrentCourseTopSectionCount = F, LinkedCourseTopSectionCount = F, SectionSchedulerCourseConstraintIDClonedFrom = F, SectionSchedulerCourseConstraintIDClonedTo = F, ScheduleFirst = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSchedulerCourseConstraintID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionSchedulerCourseConstraint", objectId = SectionSchedulerCourseConstraintID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MeetRequirements
	#'
	#' This function returns a dataframe or json object of MeetRequirements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MeetRequirement') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of MeetRequirements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMeetRequirements <- function(searchConditionsList = NULL, MeetRequirementID = F, DaysPerRotation = F, MinutesPerDay = F, AllowMultiplePeriodsPerDayRotation = F, ForceConsecutivePeriods = F, MeetRequirementIDClonedFrom = F, MeetRequirementIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, TimeSpanAnalysisType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MeetRequirement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get MeetRequirement
	#'
	#' This function returns a dataframe or json object of a MeetRequirement
	#' @param MeetRequirementID The ID of the MeetRequirement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MeetRequirement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getMeetRequirement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMeetRequirement <- function(MeetRequirementID, DaysPerRotation = F, MinutesPerDay = F, AllowMultiplePeriodsPerDayRotation = F, ForceConsecutivePeriods = F, MeetRequirementIDClonedFrom = F, MeetRequirementIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, TimeSpanAnalysisType = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MeetRequirementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "MeetRequirement", objectId = MeetRequirementID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentCourseRequests
	#'
	#' This function returns a dataframe or json object of StudentCourseRequests
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentCourseRequest') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentCourseRequests
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentCourseRequests <- function(searchConditionsList = NULL, StudentCourseRequestID = F, StudentID = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, IsAlternate = F, AlternateRank = F, StudentCourseRequestIDAlternateFor = F, StudentSectionID = F, RequestStatus = F, CourseNotScheduled = F, CourseRequested = F, CourseScheduled = F, CourseConflict = F, CourseScheduledAndInProgress = F, CourseScheduledAndIsBeforeOrInProgress = F, RequestSource = F, SchedulingMethod = F, SectionLengthSubsetSummary = F, SectionLengthSubsetCode = F, SectionLengthSubsetDescription = F, CountsAgainstRequestLimit = F, EarnedCreditsRequested = F, EarnedCreditsPossibleAnticipated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PrerequisiteMet = F, CourseScheduledAndAllTransactionsCountTowardsViewingEntity = F, RequestedFromViewingEntity = F, DisplayToViewingEntity = F, CourseNotScheduledAndIsRequestedFromViewingEntity = F, CourseScheduledAndIsRequestedFromOfferingEntityAndAllTransactionsCountTowardsViewingEntity = F, CourseScheduledAndCountsTowardsViewingEntity = F, ViewingFromOfferingEntity = F, RequestedFromOfferingEntity = F, CourseScheduledAndIsBeforeOrInProgressAndNotHasAtleastOneTransactionNotCountTowardsViewingEntity = F, CourseScheduledAndInProgressAndIsEffectiveToViewingEntity = F, CourseScheduledAndAllTransactionsCountTowardsViewingEntityAndCourseIsNotDropOrTransfer = F, CourseScheduledAndRequestedFromViewingEntityAndAllTransactionsCountTowardsViewingEntity = F, CrsIsNotDrpOrTransferAndNotScheduledOrCrsScheduledAndRequestedFromViewingEntityAndAllTransactionsCntTowardsViewingEntity = F, CourseIsNotDroppedManualOrTransfer = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentCourseRequest", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentCourseRequest
	#'
	#' This function returns a dataframe or json object of a StudentCourseRequest
	#' @param StudentCourseRequestID The ID of the StudentCourseRequest to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentCourseRequest') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentCourseRequest
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentCourseRequest <- function(StudentCourseRequestID, StudentID = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, IsAlternate = F, AlternateRank = F, StudentCourseRequestIDAlternateFor = F, StudentSectionID = F, RequestStatus = F, CourseNotScheduled = F, CourseRequested = F, CourseScheduled = F, CourseConflict = F, CourseScheduledAndInProgress = F, CourseScheduledAndIsBeforeOrInProgress = F, RequestSource = F, SchedulingMethod = F, SectionLengthSubsetSummary = F, SectionLengthSubsetCode = F, SectionLengthSubsetDescription = F, CountsAgainstRequestLimit = F, EarnedCreditsRequested = F, EarnedCreditsPossibleAnticipated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PrerequisiteMet = F, CourseScheduledAndAllTransactionsCountTowardsViewingEntity = F, RequestedFromViewingEntity = F, DisplayToViewingEntity = F, CourseNotScheduledAndIsRequestedFromViewingEntity = F, CourseScheduledAndIsRequestedFromOfferingEntityAndAllTransactionsCountTowardsViewingEntity = F, CourseScheduledAndCountsTowardsViewingEntity = F, ViewingFromOfferingEntity = F, RequestedFromOfferingEntity = F, CourseScheduledAndIsBeforeOrInProgressAndNotHasAtleastOneTransactionNotCountTowardsViewingEntity = F, CourseScheduledAndInProgressAndIsEffectiveToViewingEntity = F, CourseScheduledAndAllTransactionsCountTowardsViewingEntityAndCourseIsNotDropOrTransfer = F, CourseScheduledAndRequestedFromViewingEntityAndAllTransactionsCountTowardsViewingEntity = F, CrsIsNotDrpOrTransferAndNotScheduledOrCrsScheduledAndRequestedFromViewingEntityAndAllTransactionsCntTowardsViewingEntity = F, CourseIsNotDroppedManualOrTransfer = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentCourseRequestID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentCourseRequest", objectId = StudentCourseRequestID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseCorequisiteGroupCourses
	#'
	#' This function returns a dataframe or json object of CourseCorequisiteGroupCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseCorequisiteGroupCourse') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseCorequisiteGroupCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseCorequisiteGroupCourses <- function(searchConditionsList = NULL, CourseCorequisiteGroupCourseID = F, CourseCorequisiteGroupID = F, CourseID = F, EntityGroupKey = F, CourseCorequisiteGroupCourseIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseCorequisiteGroupCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseCorequisiteGroupCourse
	#'
	#' This function returns a dataframe or json object of a CourseCorequisiteGroupCourse
	#' @param CourseCorequisiteGroupCourseID The ID of the CourseCorequisiteGroupCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseCorequisiteGroupCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseCorequisiteGroupCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseCorequisiteGroupCourse <- function(CourseCorequisiteGroupCourseID, CourseCorequisiteGroupID = F, CourseID = F, EntityGroupKey = F, CourseCorequisiteGroupCourseIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseCorequisiteGroupCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseCorequisiteGroupCourse", objectId = CourseCorequisiteGroupCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MeetSummaries
	#'
	#' This function returns a dataframe or json object of MeetSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MeetSummary') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of MeetSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMeetSummaries <- function(searchConditionsList = NULL, MeetSummaryID = F, MeetID = F, Period = F, IsPrimary = F, Days = F, EntityIDViewing = F, CalendarID = F, IsDefaultCalendar = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasOnlyHiddenDetails = F, CalculatedPeriod = F, CalculatedDays = F, CalculatedPeriodStudent = F, CalculatedDaysStudent = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MeetSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get MeetSummary
	#'
	#' This function returns a dataframe or json object of a MeetSummary
	#' @param MeetSummaryID The ID of the MeetSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MeetSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getMeetSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMeetSummary <- function(MeetSummaryID, MeetID = F, Period = F, IsPrimary = F, Days = F, EntityIDViewing = F, CalendarID = F, IsDefaultCalendar = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasOnlyHiddenDetails = F, CalculatedPeriod = F, CalculatedDays = F, CalculatedPeriodStudent = F, CalculatedDaysStudent = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MeetSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "MeetSummary", objectId = MeetSummaryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseCustomRequirements
	#'
	#' This function returns a dataframe or json object of CourseCustomRequirements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseCustomRequirement') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseCustomRequirements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseCustomRequirements <- function(searchConditionsList = NULL, CourseCustomRequirementID = F, CourseID = F, CustomRequirementID = F, EntityGroupKey = F, CourseCustomRequirementIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseCustomRequirement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseCustomRequirement
	#'
	#' This function returns a dataframe or json object of a CourseCustomRequirement
	#' @param CourseCustomRequirementID The ID of the CourseCustomRequirement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseCustomRequirement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseCustomRequirement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseCustomRequirement <- function(CourseCustomRequirementID, CourseID = F, CustomRequirementID = F, EntityGroupKey = F, CourseCustomRequirementIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseCustomRequirementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseCustomRequirement", objectId = CourseCustomRequirementID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseCorequisiteGroups
	#'
	#' This function returns a dataframe or json object of CourseCorequisiteGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseCorequisiteGroup') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseCorequisiteGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseCorequisiteGroups <- function(searchConditionsList = NULL, CourseCorequisiteGroupID = F, Name = F, Description = F, DisplayPeriodMatch = F, StaffMatch = F, Overlap = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, CourseCorequisiteGroupIDClonedFrom = F, AutomaticRequestSetting = F, ScheduleAllCoursesInGroupOrNone = F, NameDescription = F, CourseCorequisiteGroupIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseCorequisiteGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseCorequisiteGroup
	#'
	#' This function returns a dataframe or json object of a CourseCorequisiteGroup
	#' @param CourseCorequisiteGroupID The ID of the CourseCorequisiteGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseCorequisiteGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseCorequisiteGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseCorequisiteGroup <- function(CourseCorequisiteGroupID, Name = F, Description = F, DisplayPeriodMatch = F, StaffMatch = F, Overlap = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, CourseCorequisiteGroupIDClonedFrom = F, AutomaticRequestSetting = F, ScheduleAllCoursesInGroupOrNone = F, NameDescription = F, CourseCorequisiteGroupIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseCorequisiteGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseCorequisiteGroup", objectId = CourseCorequisiteGroupID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseGroups
	#'
	#' This function returns a dataframe or json object of CourseGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseGroup') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseGroups <- function(searchConditionsList = NULL, CourseGroupID = F, Code = F, Description = F, DistrictID = F, DistrictGroupKey = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseGroup
	#'
	#' This function returns a dataframe or json object of a CourseGroup
	#' @param CourseGroupID The ID of the CourseGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseGroup <- function(CourseGroupID, Code = F, Description = F, DistrictID = F, DistrictGroupKey = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseGroup", objectId = CourseGroupID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionCorequisiteGroups
	#'
	#' This function returns a dataframe or json object of SectionCorequisiteGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionCorequisiteGroup') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionCorequisiteGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionCorequisiteGroups <- function(searchConditionsList = NULL, SectionCorequisiteGroupID = F, Name = F, Description = F, EntityID = F, AutomaticRequestSetting = F, AutomaticScheduleSetting = F, ScheduleAllSectionsInGroupOrNone = F, NameDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionCorequisiteGroupIDClonedTo = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionCorequisiteGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionCorequisiteGroup
	#'
	#' This function returns a dataframe or json object of a SectionCorequisiteGroup
	#' @param SectionCorequisiteGroupID The ID of the SectionCorequisiteGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionCorequisiteGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionCorequisiteGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionCorequisiteGroup <- function(SectionCorequisiteGroupID, Name = F, Description = F, EntityID = F, AutomaticRequestSetting = F, AutomaticScheduleSetting = F, ScheduleAllSectionsInGroupOrNone = F, NameDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionCorequisiteGroupIDClonedTo = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionCorequisiteGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionCorequisiteGroup", objectId = SectionCorequisiteGroupID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionCustomRequirements
	#'
	#' This function returns a dataframe or json object of SectionCustomRequirements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionCustomRequirement') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionCustomRequirements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionCustomRequirements <- function(searchConditionsList = NULL, SectionCustomRequirementID = F, SectionID = F, CustomRequirementID = F, SectionCustomRequirementIDClonedFrom = F, SectionCustomRequirementIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionCustomRequirement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionCustomRequirement
	#'
	#' This function returns a dataframe or json object of a SectionCustomRequirement
	#' @param SectionCustomRequirementID The ID of the SectionCustomRequirement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionCustomRequirement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionCustomRequirement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionCustomRequirement <- function(SectionCustomRequirementID, SectionID = F, CustomRequirementID = F, SectionCustomRequirementIDClonedFrom = F, SectionCustomRequirementIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionCustomRequirementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionCustomRequirement", objectId = SectionCustomRequirementID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionCorequisiteGroupSections
	#'
	#' This function returns a dataframe or json object of SectionCorequisiteGroupSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionCorequisiteGroupSection') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionCorequisiteGroupSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionCorequisiteGroupSections <- function(searchConditionsList = NULL, SectionCorequisiteGroupSectionID = F, SectionCorequisiteGroupID = F, SectionID = F, SectionCorequisiteGroupSectionIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionCorequisiteGroupSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionCorequisiteGroupSection
	#'
	#' This function returns a dataframe or json object of a SectionCorequisiteGroupSection
	#' @param SectionCorequisiteGroupSectionID The ID of the SectionCorequisiteGroupSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionCorequisiteGroupSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionCorequisiteGroupSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionCorequisiteGroupSection <- function(SectionCorequisiteGroupSectionID, SectionCorequisiteGroupID = F, SectionID = F, SectionCorequisiteGroupSectionIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionCorequisiteGroupSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionCorequisiteGroupSection", objectId = SectionCorequisiteGroupSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseGroupCourses
	#'
	#' This function returns a dataframe or json object of CourseGroupCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseGroupCourse') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseGroupCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseGroupCourses <- function(searchConditionsList = NULL, CourseGroupCourseID = F, CourseID = F, CourseGroupID = F, EntityGroupKey = F, CourseGroupCourseIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseGroupCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseGroupCourse
	#'
	#' This function returns a dataframe or json object of a CourseGroupCourse
	#' @param CourseGroupCourseID The ID of the CourseGroupCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseGroupCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseGroupCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseGroupCourse <- function(CourseGroupCourseID, CourseID = F, CourseGroupID = F, EntityGroupKey = F, CourseGroupCourseIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseGroupCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseGroupCourse", objectId = CourseGroupCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CustomRequirements
	#'
	#' This function returns a dataframe or json object of CustomRequirements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CustomRequirement') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CustomRequirements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCustomRequirements <- function(searchConditionsList = NULL, CustomRequirementID = F, DistrictID = F, Code = F, Description = F, StudentCondition = F, DistrictGroupKey = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CustomRequirement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CustomRequirement
	#'
	#' This function returns a dataframe or json object of a CustomRequirement
	#' @param CustomRequirementID The ID of the CustomRequirement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CustomRequirement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCustomRequirement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCustomRequirement <- function(CustomRequirementID, DistrictID = F, Code = F, Description = F, StudentCondition = F, DistrictGroupKey = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CustomRequirementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CustomRequirement", objectId = CustomRequirementID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseSubjects
	#'
	#' This function returns a dataframe or json object of CourseSubjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseSubject') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseSubjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseSubjects <- function(searchConditionsList = NULL, CourseSubjectID = F, Code = F, Description = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, CourseSubjectIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseSubjectIDClonedTo = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseSubject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseSubject
	#'
	#' This function returns a dataframe or json object of a CourseSubject
	#' @param CourseSubjectID The ID of the CourseSubject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseSubject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseSubject <- function(CourseSubjectID, Code = F, Description = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, CourseSubjectIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseSubjectIDClonedTo = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseSubjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseSubject", objectId = CourseSubjectID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseTypes
	#'
	#' This function returns a dataframe or json object of CourseTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseType') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseTypes <- function(searchConditionsList = NULL, CourseTypeID = F, Code = F, Description = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, CourseTypeIDClonedFrom = F, CourseTypeIDClonedTo = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseType
	#'
	#' This function returns a dataframe or json object of a CourseType
	#' @param CourseTypeID The ID of the CourseType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseType <- function(CourseTypeID, Code = F, Description = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, CourseTypeIDClonedFrom = F, CourseTypeIDClonedTo = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseType", objectId = CourseTypeID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseLengths
	#'
	#' This function returns a dataframe or json object of CourseLengths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseLength') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseLengths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseLengths <- function(searchConditionsList = NULL, CourseLengthID = F, EntityGroupKey = F, Code = F, Description = F, DefaultEarnedCredits = F, EntityID = F, SchoolYearID = F, CourseLengthIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseLengthIDClonedTo = F, SectionLengthCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseLength", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseLength
	#'
	#' This function returns a dataframe or json object of a CourseLength
	#' @param CourseLengthID The ID of the CourseLength to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseLength') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseLength
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseLength <- function(CourseLengthID, EntityGroupKey = F, Code = F, Description = F, DefaultEarnedCredits = F, EntityID = F, SchoolYearID = F, CourseLengthIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseLengthIDClonedTo = F, SectionLengthCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseLengthID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseLength", objectId = CourseLengthID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionDefaults
	#'
	#' This function returns a dataframe or json object of SectionDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionDefault') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionDefaults <- function(searchConditionsList = NULL, SectionDefaultID = F, CourseID = F, MinimumStudentCount = F, MaximumStudentCount = F, OptimalStudentCount = F, SectionDefaultIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionDefault
	#'
	#' This function returns a dataframe or json object of a SectionDefault
	#' @param SectionDefaultID The ID of the SectionDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionDefault <- function(SectionDefaultID, CourseID = F, MinimumStudentCount = F, MaximumStudentCount = F, OptimalStudentCount = F, SectionDefaultIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionDefault", objectId = SectionDefaultID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Meets
	#'
	#' This function returns a dataframe or json object of Meets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Meet') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of Meets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMeets <- function(searchConditionsList = NULL, MeetID = F, SectionID = F, EntityID = F, SchoolYearID = F, RoomID = F, Type = F, StartDate = F, EndDate = F, OverridePeriodStartTime = F, OverridePeriodEndTime = F, IsPrimary = F, LockAllMeetDisplayPeriodsFromSectionScheduler = F, MeetIDClonedFrom = F, DurationInMinutes = F, TotalMeetDisplayPeriodCount = F, IsAssignedToAHomeroomRoom = F, TotalStaffMeetCount = F, MeetIDClonedTo = F, ViewingFromOfferingEntity = F, StartDateEndDateBuildingCodeRoomNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayPeriodRotationID = F, HasDisplayPeriodRotationAssigned = F, LockAllStaffMeetsFromSectionScheduler = F, LockRoomFromSectionScheduler = F, LockAllMeetDayRotationsFromSectionScheduler = F, CalendarIDStaff = F, RoomSeatsAvailable = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "Meet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get Meet
	#'
	#' This function returns a dataframe or json object of a Meet
	#' @param MeetID The ID of the Meet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Meet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMeet <- function(MeetID, SectionID = F, EntityID = F, SchoolYearID = F, RoomID = F, Type = F, StartDate = F, EndDate = F, OverridePeriodStartTime = F, OverridePeriodEndTime = F, IsPrimary = F, LockAllMeetDisplayPeriodsFromSectionScheduler = F, MeetIDClonedFrom = F, DurationInMinutes = F, TotalMeetDisplayPeriodCount = F, IsAssignedToAHomeroomRoom = F, TotalStaffMeetCount = F, MeetIDClonedTo = F, ViewingFromOfferingEntity = F, StartDateEndDateBuildingCodeRoomNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayPeriodRotationID = F, HasDisplayPeriodRotationAssigned = F, LockAllStaffMeetsFromSectionScheduler = F, LockRoomFromSectionScheduler = F, LockAllMeetDayRotationsFromSectionScheduler = F, CalendarIDStaff = F, RoomSeatsAvailable = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MeetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "Meet", objectId = MeetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MeetDisplayPeriods
	#'
	#' This function returns a dataframe or json object of MeetDisplayPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MeetDisplayPeriod') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of MeetDisplayPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMeetDisplayPeriods <- function(searchConditionsList = NULL, MeetDisplayPeriodID = F, MeetID = F, DisplayPeriodID = F, IsPrimary = F, HideMeetDisplayPeriod = F, LockDisplayPeriod = F, ScheduledBySectionScheduler = F, MeetDisplayPeriodIDClonedFrom = F, ViewingFromOfferingEntity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MeetDisplayPeriodStartTime = F, MeetDisplayPeriodEndTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MeetDisplayPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get MeetDisplayPeriod
	#'
	#' This function returns a dataframe or json object of a MeetDisplayPeriod
	#' @param MeetDisplayPeriodID The ID of the MeetDisplayPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MeetDisplayPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getMeetDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMeetDisplayPeriod <- function(MeetDisplayPeriodID, MeetID = F, DisplayPeriodID = F, IsPrimary = F, HideMeetDisplayPeriod = F, LockDisplayPeriod = F, ScheduledBySectionScheduler = F, MeetDisplayPeriodIDClonedFrom = F, ViewingFromOfferingEntity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MeetDisplayPeriodStartTime = F, MeetDisplayPeriodEndTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MeetDisplayPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "MeetDisplayPeriod", objectId = MeetDisplayPeriodID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StaffMeets
	#'
	#' This function returns a dataframe or json object of StaffMeets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StaffMeet') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StaffMeets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStaffMeets <- function(searchConditionsList = NULL, StaffMeetID = F, MeetID = F, StaffID = F, IsPrimary = F, CanMakeClosedGradingPeriodGradeChange = F, ApplyClosedGradingPeriodGradeChangePermission = F, NameMeetDescription = F, HasGradebookAccess = F, HasAttendanceAccess = F, EffectiveStartDate = F, EffectiveEndDate = F, GradebookLastAccessedTime = F, StaffMeetIDClonedFrom = F, MeetsToday = F, ViewingFromOfferingEntity = F, MeetIsCurrent = F, AssignmentCount = F, ClosedGradingPeriodGradeChangeCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsSubstitute = F, StaffMeetIDSubstituteFor = F, HasAttendanceAccessAsOfDate = F, SectionID = F, SchoolYearID = F, IsStaffCertified = F, ScheduledBySectionScheduler = F, LockStaffFromSectionScheduler = F, IsLongTermSubstitute = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StaffMeet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StaffMeet
	#'
	#' This function returns a dataframe or json object of a StaffMeet
	#' @param StaffMeetID The ID of the StaffMeet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StaffMeet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStaffMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStaffMeet <- function(StaffMeetID, MeetID = F, StaffID = F, IsPrimary = F, CanMakeClosedGradingPeriodGradeChange = F, ApplyClosedGradingPeriodGradeChangePermission = F, NameMeetDescription = F, HasGradebookAccess = F, HasAttendanceAccess = F, EffectiveStartDate = F, EffectiveEndDate = F, GradebookLastAccessedTime = F, StaffMeetIDClonedFrom = F, MeetsToday = F, ViewingFromOfferingEntity = F, MeetIsCurrent = F, AssignmentCount = F, ClosedGradingPeriodGradeChangeCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsSubstitute = F, StaffMeetIDSubstituteFor = F, HasAttendanceAccessAsOfDate = F, SectionID = F, SchoolYearID = F, IsStaffCertified = F, ScheduledBySectionScheduler = F, LockStaffFromSectionScheduler = F, IsLongTermSubstitute = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StaffMeetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StaffMeet", objectId = StaffMeetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSchedulingCategories
	#'
	#' This function returns a dataframe or json object of SectionSchedulingCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulingCategory') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionSchedulingCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSchedulingCategories <- function(searchConditionsList = NULL, SectionSchedulingCategoryID = F, SchedulingCategoryID = F, SectionID = F, SectionSchedulingCategoryIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulingCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSchedulingCategory
	#'
	#' This function returns a dataframe or json object of a SectionSchedulingCategory
	#' @param SectionSchedulingCategoryID The ID of the SectionSchedulingCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulingCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionSchedulingCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSchedulingCategory <- function(SectionSchedulingCategoryID, SchedulingCategoryID = F, SectionID = F, SectionSchedulingCategoryIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSchedulingCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionSchedulingCategory", objectId = SectionSchedulingCategoryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchedulingPeriodDisplayPeriods
	#'
	#' This function returns a dataframe or json object of SchedulingPeriodDisplayPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingPeriodDisplayPeriod') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SchedulingPeriodDisplayPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchedulingPeriodDisplayPeriods <- function(searchConditionsList = NULL, SchedulingPeriodDisplayPeriodID = F, SchedulingPeriodID = F, DisplayPeriodID = F, EntityGroupKey = F, SchedulingPeriodDisplayPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingPeriodDisplayPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SchedulingPeriodDisplayPeriod
	#'
	#' This function returns a dataframe or json object of a SchedulingPeriodDisplayPeriod
	#' @param SchedulingPeriodDisplayPeriodID The ID of the SchedulingPeriodDisplayPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingPeriodDisplayPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSchedulingPeriodDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchedulingPeriodDisplayPeriod <- function(SchedulingPeriodDisplayPeriodID, SchedulingPeriodID = F, DisplayPeriodID = F, EntityGroupKey = F, SchedulingPeriodDisplayPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchedulingPeriodDisplayPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SchedulingPeriodDisplayPeriod", objectId = SchedulingPeriodDisplayPeriodID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentEntityYearSchedulingCategories
	#'
	#' This function returns a dataframe or json object of StudentEntityYearSchedulingCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentEntityYearSchedulingCategory') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentEntityYearSchedulingCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentEntityYearSchedulingCategories <- function(searchConditionsList = NULL, StudentEntityYearSchedulingCategoryID = F, SchedulingCategoryID = F, StudentEntityYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentEntityYearSchedulingCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentEntityYearSchedulingCategory
	#'
	#' This function returns a dataframe or json object of a StudentEntityYearSchedulingCategory
	#' @param StudentEntityYearSchedulingCategoryID The ID of the StudentEntityYearSchedulingCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentEntityYearSchedulingCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentEntityYearSchedulingCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentEntityYearSchedulingCategory <- function(StudentEntityYearSchedulingCategoryID, SchedulingCategoryID = F, StudentEntityYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentEntityYearSchedulingCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentEntityYearSchedulingCategory", objectId = StudentEntityYearSchedulingCategoryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentSectionTransactions
	#'
	#' This function returns a dataframe or json object of StudentSectionTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentSectionTransaction') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentSectionTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentSectionTransactions <- function(searchConditionsList = NULL, StudentSectionTransactionID = F, StudentSectionID = F, SectionLengthSubsetID = F, EntityIDCountsAs = F, StartDate = F, EndDate = F, NameIDRequestedBy = F, EarlyExitReasonID = F, HideNewStudentButton = F, SectionID = F, IsCECE = F, CalendarID = F, IsInProgressAsOfToday = F, EndsAfterSectionLengthStartDate = F, IsInProgress = F, OverlapsSectionLength = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CountsTowardsViewingEntity = F, StudentID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentSectionTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentSectionTransaction
	#'
	#' This function returns a dataframe or json object of a StudentSectionTransaction
	#' @param StudentSectionTransactionID The ID of the StudentSectionTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentSectionTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentSectionTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentSectionTransaction <- function(StudentSectionTransactionID, StudentSectionID = F, SectionLengthSubsetID = F, EntityIDCountsAs = F, StartDate = F, EndDate = F, NameIDRequestedBy = F, EarlyExitReasonID = F, HideNewStudentButton = F, SectionID = F, IsCECE = F, CalendarID = F, IsInProgressAsOfToday = F, EndsAfterSectionLengthStartDate = F, IsInProgress = F, OverlapsSectionLength = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CountsTowardsViewingEntity = F, StudentID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentSectionTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentSectionTransaction", objectId = StudentSectionTransactionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentSections
	#'
	#' This function returns a dataframe or json object of StudentSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentSection') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentSections <- function(searchConditionsList = NULL, StudentSectionID = F, StudentID = F, SectionID = F, ExcludeFromReportCardsAndTranscripts = F, IsTransferCourse = F, UseEarnedCreditOverride = F, EarnedCreditOverride = F, RenderTransferGradesRowButton = F, StudentSectionLink = F, ExcludeFromStudentSectionLink = F, EntityIDCourse = F, SchoolYearIDCourse = F, SectionLengthSubsetSummary = F, FailedCredits = F, EarnedCredits = F, EarnedCreditAttempted = F, LinkedStudentSectionsEarnedCredit = F, LinkedStudentSectionsFailedCredit = F, LinkedStudentSectionsEarnedCreditAttempted = F, DisplayLinkedStudentSection = F, IsWorkInProgress = F, UseEarnedCreditTotalOverride = F, TotalEarnedCreditOverride = F, TotalFailedCreditOverride = F, StartDate = F, EndDate = F, MultipleTransactions = F, Status = F, LastStudentSectionTransactionConsideredDropped = F, InProgress = F, IsBeforeOrInProgress = F, EarnedCreditsPossible = F, MissingAssignmentCount = F, IsFlaggedAsMissingAssignmentCount = F, UnscoredPastDueAssignmentCount = F, FilteredMissingAssignmentCount = F, FilteredGradedAssignmentCount = F, FilteredUnGradedAssignmentCount = F, StudentSectionNoteCountForCurrentUser = F, TotalDaysAbsent = F, TotalDaysExcused = F, TotalDaysOther = F, TotalDaysUnexcused = F, TotalDaysTardy = F, GradebookStudentNameToUse = F, HasLinkingConflicts = F, ViewingFromOfferingEntity = F, CountsForViewingEntity = F, TransactionCountsForViewingEntity = F, DisplayToViewingEntity = F, GradeReferenceID = F, TransferCourseName = F, StudentSectionCode = F, CourseOrTransferDescription = F, IsCurrentStudentSection = F, AssignmentDueDateAttendance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasAtLeastOneCrossEntityStudentSectionTransaction = F, StudentSectionMNID = F, IsTSAProficient = F, HasStudentSectionNoteForCurrentUser = F, IsStudentSectionScheduledToMeet = F, IsForCurrentSchoolYear = F, IsActiveAsOfDate = F, IsActiveForTodayOrForSectionStartOrEnd = F, GradebookSortCode = F, HasStudentGradingScaleGroupForGradingPeriodGradeBucket = F, ActiveStudentGroups = F, IsAvailableForAssignmentStudentGroup = F, IsEffectiveForViewingEntity = F, HasAtleastOneTransactionNotCountTowardsViewingEntity = F, AllTransactionsCountTowardsViewingEntity = F, IsBeforeOrInProgressAndNotHasAtleastOneTransactionNotCountTowardsViewingEntity = F, RequestedFromOfferingEntityAndAllTransactionsCountTowardsViewingEntity = F, InProgressAndIsEffectiveForViewingEntity = F, ExistsStudentSectionGPAMethods = F, RequestedFromViewingEntityAndAllTransactionsCountTowardsViewingEntity = F, EarnedCreditsCompleted = F, FailedCreditsCompleted = F, EarnedCreditAttemptedCompleted = F, EarnedCreditsPossibleCompleted = F, EarnedCreditsAllEntered = F, FailedCreditsAllEntered = F, EarnedCreditAttemptedAllEntered = F, EarnedCreditsPossibleAllEntered = F, ConversionKey = F, StudentSectionTypeOverride = F, IsAlternativeCreditCourse = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentSection
	#'
	#' This function returns a dataframe or json object of a StudentSection
	#' @param StudentSectionID The ID of the StudentSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentSection <- function(StudentSectionID, StudentID = F, SectionID = F, ExcludeFromReportCardsAndTranscripts = F, IsTransferCourse = F, UseEarnedCreditOverride = F, EarnedCreditOverride = F, RenderTransferGradesRowButton = F, StudentSectionLink = F, ExcludeFromStudentSectionLink = F, EntityIDCourse = F, SchoolYearIDCourse = F, SectionLengthSubsetSummary = F, FailedCredits = F, EarnedCredits = F, EarnedCreditAttempted = F, LinkedStudentSectionsEarnedCredit = F, LinkedStudentSectionsFailedCredit = F, LinkedStudentSectionsEarnedCreditAttempted = F, DisplayLinkedStudentSection = F, IsWorkInProgress = F, UseEarnedCreditTotalOverride = F, TotalEarnedCreditOverride = F, TotalFailedCreditOverride = F, StartDate = F, EndDate = F, MultipleTransactions = F, Status = F, LastStudentSectionTransactionConsideredDropped = F, InProgress = F, IsBeforeOrInProgress = F, EarnedCreditsPossible = F, MissingAssignmentCount = F, IsFlaggedAsMissingAssignmentCount = F, UnscoredPastDueAssignmentCount = F, FilteredMissingAssignmentCount = F, FilteredGradedAssignmentCount = F, FilteredUnGradedAssignmentCount = F, StudentSectionNoteCountForCurrentUser = F, TotalDaysAbsent = F, TotalDaysExcused = F, TotalDaysOther = F, TotalDaysUnexcused = F, TotalDaysTardy = F, GradebookStudentNameToUse = F, HasLinkingConflicts = F, ViewingFromOfferingEntity = F, CountsForViewingEntity = F, TransactionCountsForViewingEntity = F, DisplayToViewingEntity = F, GradeReferenceID = F, TransferCourseName = F, StudentSectionCode = F, CourseOrTransferDescription = F, IsCurrentStudentSection = F, AssignmentDueDateAttendance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasAtLeastOneCrossEntityStudentSectionTransaction = F, StudentSectionMNID = F, IsTSAProficient = F, HasStudentSectionNoteForCurrentUser = F, IsStudentSectionScheduledToMeet = F, IsForCurrentSchoolYear = F, IsActiveAsOfDate = F, IsActiveForTodayOrForSectionStartOrEnd = F, GradebookSortCode = F, HasStudentGradingScaleGroupForGradingPeriodGradeBucket = F, ActiveStudentGroups = F, IsAvailableForAssignmentStudentGroup = F, IsEffectiveForViewingEntity = F, HasAtleastOneTransactionNotCountTowardsViewingEntity = F, AllTransactionsCountTowardsViewingEntity = F, IsBeforeOrInProgressAndNotHasAtleastOneTransactionNotCountTowardsViewingEntity = F, RequestedFromOfferingEntityAndAllTransactionsCountTowardsViewingEntity = F, InProgressAndIsEffectiveForViewingEntity = F, ExistsStudentSectionGPAMethods = F, RequestedFromViewingEntityAndAllTransactionsCountTowardsViewingEntity = F, EarnedCreditsCompleted = F, FailedCreditsCompleted = F, EarnedCreditAttemptedCompleted = F, EarnedCreditsPossibleCompleted = F, EarnedCreditsAllEntered = F, FailedCreditsAllEntered = F, EarnedCreditAttemptedAllEntered = F, EarnedCreditsPossibleAllEntered = F, ConversionKey = F, StudentSectionTypeOverride = F, IsAlternativeCreditCourse = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentSection", objectId = StudentSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchedulingTeams
	#'
	#' This function returns a dataframe or json object of SchedulingTeams
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingTeam') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SchedulingTeams
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchedulingTeams <- function(searchConditionsList = NULL, SchedulingTeamID = F, Code = F, Description = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, SchedulingTeamIDClonedFrom = F, CodeDescription = F, SchedulingTeamIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingTeam", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SchedulingTeam
	#'
	#' This function returns a dataframe or json object of a SchedulingTeam
	#' @param SchedulingTeamID The ID of the SchedulingTeam to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingTeam') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSchedulingTeam
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchedulingTeam <- function(SchedulingTeamID, Code = F, Description = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, SchedulingTeamIDClonedFrom = F, CodeDescription = F, SchedulingTeamIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchedulingTeamID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SchedulingTeam", objectId = SchedulingTeamID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchedulingPeriods
	#'
	#' This function returns a dataframe or json object of SchedulingPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingPeriod') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SchedulingPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchedulingPeriods <- function(searchConditionsList = NULL, SchedulingPeriodID = F, DayRotationID = F, CodeNumber = F, EntityGroupKey = F, SchedulingPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SchedulingPeriodIDClonedTo = F, DynamicRelationshipID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SchedulingPeriod
	#'
	#' This function returns a dataframe or json object of a SchedulingPeriod
	#' @param SchedulingPeriodID The ID of the SchedulingPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSchedulingPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchedulingPeriod <- function(SchedulingPeriodID, DayRotationID = F, CodeNumber = F, EntityGroupKey = F, SchedulingPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SchedulingPeriodIDClonedTo = F, DynamicRelationshipID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchedulingPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SchedulingPeriod", objectId = SchedulingPeriodID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchedulingCategories
	#'
	#' This function returns a dataframe or json object of SchedulingCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingCategory') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SchedulingCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchedulingCategories <- function(searchConditionsList = NULL, SchedulingCategoryID = F, Code = F, Description = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, SchedulingCategoryIDClonedFrom = F, CodeDescription = F, SchedulingCategoryIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SchedulingCategory
	#'
	#' This function returns a dataframe or json object of a SchedulingCategory
	#' @param SchedulingCategoryID The ID of the SchedulingCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSchedulingCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchedulingCategory <- function(SchedulingCategoryID, Code = F, Description = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, SchedulingCategoryIDClonedFrom = F, CodeDescription = F, SchedulingCategoryIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchedulingCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SchedulingCategory", objectId = SchedulingCategoryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisplayPeriodConflicts
	#'
	#' This function returns a dataframe or json object of DisplayPeriodConflicts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisplayPeriodConflict') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of DisplayPeriodConflicts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisplayPeriodConflicts <- function(searchConditionsList = NULL, DisplayPeriodConflictID = F, DisplayPeriodIDBase = F, DisplayPeriodIDConflicting = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "DisplayPeriodConflict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisplayPeriodConflict
	#'
	#' This function returns a dataframe or json object of a DisplayPeriodConflict
	#' @param DisplayPeriodConflictID The ID of the DisplayPeriodConflict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisplayPeriodConflict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getDisplayPeriodConflict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisplayPeriodConflict <- function(DisplayPeriodConflictID, DisplayPeriodIDBase = F, DisplayPeriodIDConflicting = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisplayPeriodConflictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "DisplayPeriodConflict", objectId = DisplayPeriodConflictID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisplayPeriods
	#'
	#' This function returns a dataframe or json object of DisplayPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisplayPeriod') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of DisplayPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisplayPeriods <- function(searchConditionsList = NULL, DisplayPeriodID = F, DayRotationID = F, SortNumber = F, Code = F, Description = F, AttendancePeriodID = F, EntityGroupKey = F, DisplayPeriodIDClonedFrom = F, IsOutsideRegularSchoolDay = F, IsLunchPeriod = F, TeachingHourEquivalent = F, CodeDescription = F, CodeDescriptionDayRotation = F, DisplayPeriodStartTime = F, DisplayPeriodEndTime = F, DisplayPeriodIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayPeriodCodeDayRotationCode = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "DisplayPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisplayPeriod
	#'
	#' This function returns a dataframe or json object of a DisplayPeriod
	#' @param DisplayPeriodID The ID of the DisplayPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisplayPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisplayPeriod <- function(DisplayPeriodID, DayRotationID = F, SortNumber = F, Code = F, Description = F, AttendancePeriodID = F, EntityGroupKey = F, DisplayPeriodIDClonedFrom = F, IsOutsideRegularSchoolDay = F, IsLunchPeriod = F, TeachingHourEquivalent = F, CodeDescription = F, CodeDescriptionDayRotation = F, DisplayPeriodStartTime = F, DisplayPeriodEndTime = F, DisplayPeriodIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayPeriodCodeDayRotationCode = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisplayPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "DisplayPeriod", objectId = DisplayPeriodID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DayRotations
	#'
	#' This function returns a dataframe or json object of DayRotations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DayRotation') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of DayRotations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDayRotations <- function(searchConditionsList = NULL, DayRotationID = F, SortNumber = F, Code = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, ConsecutiveTeachingHourLimit = F, MaximumTeachingHourLimit = F, DayRotationIDClonedFrom = F, DayRotationIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "DayRotation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DayRotation
	#'
	#' This function returns a dataframe or json object of a DayRotation
	#' @param DayRotationID The ID of the DayRotation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DayRotation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getDayRotation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDayRotation <- function(DayRotationID, SortNumber = F, Code = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, ConsecutiveTeachingHourLimit = F, MaximumTeachingHourLimit = F, DayRotationIDClonedFrom = F, DayRotationIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DayRotationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "DayRotation", objectId = DayRotationID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseSectionLengthExcludes
	#'
	#' This function returns a dataframe or json object of CourseSectionLengthExcludes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseSectionLengthExclude') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseSectionLengthExcludes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseSectionLengthExcludes <- function(searchConditionsList = NULL, CourseSectionLengthExcludeID = F, CourseID = F, SectionLengthID = F, DistributionPercent = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseSectionLengthExclude", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseSectionLengthExclude
	#'
	#' This function returns a dataframe or json object of a CourseSectionLengthExclude
	#' @param CourseSectionLengthExcludeID The ID of the CourseSectionLengthExclude to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseSectionLengthExclude') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseSectionLengthExclude
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseSectionLengthExclude <- function(CourseSectionLengthExcludeID, CourseID = F, SectionLengthID = F, DistributionPercent = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseSectionLengthExcludeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseSectionLengthExclude", objectId = CourseSectionLengthExcludeID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCourseEntityOfferedToSections
	#'
	#' This function returns a dataframe or json object of TempCourseEntityOfferedToSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourseEntityOfferedToSection') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempCourseEntityOfferedToSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCourseEntityOfferedToSections <- function(searchConditionsList = NULL, TempCourseEntityOfferedToSectionID = F, CourseEntityOfferedToSectionID = F, CourseEntityOfferedToID = F, EntityOfferedToCode = F, EntityOfferedToName = F, SectionID = F, SectionCode = F, SectionIsActive = F, SectionMaximumStudentCount = F, SectionReservedSeatCount = F, SectionSectionLengthCode = F, IsActiveOverride = F, MaximumStudentCount = F, SeatsAvailable = F, OriginalMaximumStudentCount = F, CourseEntityOfferedToSectionRecordExists = F, RowIsSelected = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, EntityIDOfferedTo = F, SchoolYearID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempCourseEntityOfferedToSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempCourseEntityOfferedToSection
	#'
	#' This function returns a dataframe or json object of a TempCourseEntityOfferedToSection
	#' @param TempCourseEntityOfferedToSectionID The ID of the TempCourseEntityOfferedToSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourseEntityOfferedToSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempCourseEntityOfferedToSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCourseEntityOfferedToSection <- function(TempCourseEntityOfferedToSectionID, CourseEntityOfferedToSectionID = F, CourseEntityOfferedToID = F, EntityOfferedToCode = F, EntityOfferedToName = F, SectionID = F, SectionCode = F, SectionIsActive = F, SectionMaximumStudentCount = F, SectionReservedSeatCount = F, SectionSectionLengthCode = F, IsActiveOverride = F, MaximumStudentCount = F, SeatsAvailable = F, OriginalMaximumStudentCount = F, CourseEntityOfferedToSectionRecordExists = F, RowIsSelected = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, EntityIDOfferedTo = F, SchoolYearID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCourseEntityOfferedToSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempCourseEntityOfferedToSection", objectId = TempCourseEntityOfferedToSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MeetSummaryDetails
	#'
	#' This function returns a dataframe or json object of MeetSummaryDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MeetSummaryDetail') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of MeetSummaryDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMeetSummaryDetails <- function(searchConditionsList = NULL, MeetSummaryDetailID = F, DisplayPeriodID = F, MeetSummaryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsPrimary = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MeetSummaryDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get MeetSummaryDetail
	#'
	#' This function returns a dataframe or json object of a MeetSummaryDetail
	#' @param MeetSummaryDetailID The ID of the MeetSummaryDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MeetSummaryDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getMeetSummaryDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMeetSummaryDetail <- function(MeetSummaryDetailID, DisplayPeriodID = F, MeetSummaryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsPrimary = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MeetSummaryDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "MeetSummaryDetail", objectId = MeetSummaryDetailID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSubstituteAssignments
	#'
	#' This function returns a dataframe or json object of TempSubstituteAssignments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSubstituteAssignment') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempSubstituteAssignments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSubstituteAssignments <- function(searchConditionsList = NULL, TempSubstituteAssignmentID = F, SubstituteStaffID = F, SourceStaffID = F, MeetID = F, CourseCodeSectionCode = F, Period = F, Date = F, HasGradebookAccess = F, HasAttendanceAccess = F, DisplayPeriodSortNumber = F, DisplayPeriodID = F, StaffMeetID = F, EntityID = F, SchoolYearID = F, Conflicts = F, SectionAlreadyCovered = F, StaffName = F, HasConflicts = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionID = F, IsLongTermSubstitute = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempSubstituteAssignment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempSubstituteAssignment
	#'
	#' This function returns a dataframe or json object of a TempSubstituteAssignment
	#' @param TempSubstituteAssignmentID The ID of the TempSubstituteAssignment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempSubstituteAssignment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempSubstituteAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSubstituteAssignment <- function(TempSubstituteAssignmentID, SubstituteStaffID = F, SourceStaffID = F, MeetID = F, CourseCodeSectionCode = F, Period = F, Date = F, HasGradebookAccess = F, HasAttendanceAccess = F, DisplayPeriodSortNumber = F, DisplayPeriodID = F, StaffMeetID = F, EntityID = F, SchoolYearID = F, Conflicts = F, SectionAlreadyCovered = F, StaffName = F, HasConflicts = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionID = F, IsLongTermSubstitute = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSubstituteAssignmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempSubstituteAssignment", objectId = TempSubstituteAssignmentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisplayPeriodRotationDisplayPeriods
	#'
	#' This function returns a dataframe or json object of DisplayPeriodRotationDisplayPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisplayPeriodRotationDisplayPeriod') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of DisplayPeriodRotationDisplayPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisplayPeriodRotationDisplayPeriods <- function(searchConditionsList = NULL, DisplayPeriodRotationDisplayPeriodID = F, DisplayPeriodRotationID = F, DisplayPeriodID = F, IsPrimary = F, HideMeetDisplayPeriod = F, DisplayPeriodRotationDisplayPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "DisplayPeriodRotationDisplayPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisplayPeriodRotationDisplayPeriod
	#'
	#' This function returns a dataframe or json object of a DisplayPeriodRotationDisplayPeriod
	#' @param DisplayPeriodRotationDisplayPeriodID The ID of the DisplayPeriodRotationDisplayPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisplayPeriodRotationDisplayPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getDisplayPeriodRotationDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisplayPeriodRotationDisplayPeriod <- function(DisplayPeriodRotationDisplayPeriodID, DisplayPeriodRotationID = F, DisplayPeriodID = F, IsPrimary = F, HideMeetDisplayPeriod = F, DisplayPeriodRotationDisplayPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisplayPeriodRotationDisplayPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "DisplayPeriodRotationDisplayPeriod", objectId = DisplayPeriodRotationDisplayPeriodID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisplayPeriodRotations
	#'
	#' This function returns a dataframe or json object of DisplayPeriodRotations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisplayPeriodRotation') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of DisplayPeriodRotations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisplayPeriodRotations <- function(searchConditionsList = NULL, DisplayPeriodRotationID = F, EntityID = F, SchoolYearID = F, Code = F, Description = F, DisplayPeriodSummary = F, DisplayPeriodRotationIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayPeriodRotationIDClonedTo = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "DisplayPeriodRotation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisplayPeriodRotation
	#'
	#' This function returns a dataframe or json object of a DisplayPeriodRotation
	#' @param DisplayPeriodRotationID The ID of the DisplayPeriodRotation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisplayPeriodRotation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getDisplayPeriodRotation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisplayPeriodRotation <- function(DisplayPeriodRotationID, EntityID = F, SchoolYearID = F, Code = F, Description = F, DisplayPeriodSummary = F, DisplayPeriodRotationIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayPeriodRotationIDClonedTo = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisplayPeriodRotationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "DisplayPeriodRotation", objectId = DisplayPeriodRotationID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AvailabilityFilterCourseStudentStudents
	#'
	#' This function returns a dataframe or json object of AvailabilityFilterCourseStudentStudents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AvailabilityFilterCourseStudentStudent') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of AvailabilityFilterCourseStudentStudents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAvailabilityFilterCourseStudentStudents <- function(searchConditionsList = NULL, AvailabilityFilterCourseStudentStudentID = F, AvailabilityFilterCourseStudentID = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ArenaSchedulingStatus = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AvailabilityFilterCourseStudentStudent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get AvailabilityFilterCourseStudentStudent
	#'
	#' This function returns a dataframe or json object of an AvailabilityFilterCourseStudentStudent
	#' @param AvailabilityFilterCourseStudentStudentID The ID of the AvailabilityFilterCourseStudentStudent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AvailabilityFilterCourseStudentStudent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getAvailabilityFilterCourseStudentStudent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAvailabilityFilterCourseStudentStudent <- function(AvailabilityFilterCourseStudentStudentID, AvailabilityFilterCourseStudentID = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ArenaSchedulingStatus = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AvailabilityFilterCourseStudentStudentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "AvailabilityFilterCourseStudentStudent", objectId = AvailabilityFilterCourseStudentStudentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AvailabilityFilterCourseStudentCourses
	#'
	#' This function returns a dataframe or json object of AvailabilityFilterCourseStudentCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AvailabilityFilterCourseStudentCourse') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of AvailabilityFilterCourseStudentCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAvailabilityFilterCourseStudentCourses <- function(searchConditionsList = NULL, AvailabilityFilterCourseStudentCourseID = F, AvailabilityFilterCourseStudentID = F, CourseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AvailabilityFilterCourseStudentCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get AvailabilityFilterCourseStudentCourse
	#'
	#' This function returns a dataframe or json object of an AvailabilityFilterCourseStudentCourse
	#' @param AvailabilityFilterCourseStudentCourseID The ID of the AvailabilityFilterCourseStudentCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AvailabilityFilterCourseStudentCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getAvailabilityFilterCourseStudentCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAvailabilityFilterCourseStudentCourse <- function(AvailabilityFilterCourseStudentCourseID, AvailabilityFilterCourseStudentID = F, CourseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AvailabilityFilterCourseStudentCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "AvailabilityFilterCourseStudentCourse", objectId = AvailabilityFilterCourseStudentCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDayRotations
	#'
	#' This function returns a dataframe or json object of TempDayRotations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempDayRotation') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempDayRotations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDayRotations <- function(searchConditionsList = NULL, TempDayRotationID = F, DayRotationID = F, Code = F, ObjectIsDirty = F, ObjectName = F, Note = F, RecordsUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempDayRotation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempDayRotation
	#'
	#' This function returns a dataframe or json object of a TempDayRotation
	#' @param TempDayRotationID The ID of the TempDayRotation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempDayRotation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempDayRotation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDayRotation <- function(TempDayRotationID, DayRotationID = F, Code = F, ObjectIsDirty = F, ObjectName = F, Note = F, RecordsUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDayRotationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempDayRotation", objectId = TempDayRotationID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFailedDayRotations
	#'
	#' This function returns a dataframe or json object of TempFailedDayRotations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedDayRotation') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempFailedDayRotations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFailedDayRotations <- function(searchConditionsList = NULL, TempFailedDayRotationID = F, TempDayRotationID = F, DayRotationID = F, Code = F, ObjectIsDirty = F, ObjectName = F, Note = F, RecordsUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedDayRotation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempFailedDayRotation
	#'
	#' This function returns a dataframe or json object of a TempFailedDayRotation
	#' @param TempFailedDayRotationID The ID of the TempFailedDayRotation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedDayRotation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempFailedDayRotation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFailedDayRotation <- function(TempFailedDayRotationID, TempDayRotationID = F, DayRotationID = F, Code = F, ObjectIsDirty = F, ObjectName = F, Note = F, RecordsUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFailedDayRotationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempFailedDayRotation", objectId = TempFailedDayRotationID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchedulingConfigEntityYears
	#'
	#' This function returns a dataframe or json object of SchedulingConfigEntityYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingConfigEntityYear') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SchedulingConfigEntityYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchedulingConfigEntityYears <- function(searchConditionsList = NULL, ConfigEntityYearID = F, EntityID = F, SchoolYearID = F, ConfigEntityYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EnableValidationOfRoomCapacityDuringStudentSectionEnrollment = F, EnableArenaSchedulingConfirmationScreen = F, ArenaSchedulingConfirmationScreenMessage = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "ConfigEntityYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SchedulingConfigEntityYear
	#'
	#' This function returns a dataframe or json object of a SchedulingConfigEntityYear
	#' @param SchedulingConfigEntityYearID The ID of the SchedulingConfigEntityYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingConfigEntityYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSchedulingConfigEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchedulingConfigEntityYear <- function(SchedulingConfigEntityYearID, ConfigEntityYearID = F, EntityID = F, SchoolYearID = F, ConfigEntityYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EnableValidationOfRoomCapacityDuringStudentSectionEnrollment = F, EnableArenaSchedulingConfirmationScreen = F, ArenaSchedulingConfirmationScreenMessage = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchedulingConfigEntityYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "ConfigEntityYear", objectId = SchedulingConfigEntityYearID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchedulingBoardFilters
	#'
	#' This function returns a dataframe or json object of SchedulingBoardFilters
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingBoardFilter') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SchedulingBoardFilters
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchedulingBoardFilters <- function(searchConditionsList = NULL, SchedulingBoardFilterID = F, Description = F, DisplayOrder = F, Type = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingBoardFilter", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SchedulingBoardFilter
	#'
	#' This function returns a dataframe or json object of a SchedulingBoardFilter
	#' @param SchedulingBoardFilterID The ID of the SchedulingBoardFilter to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingBoardFilter') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSchedulingBoardFilter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchedulingBoardFilter <- function(SchedulingBoardFilterID, Description = F, DisplayOrder = F, Type = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchedulingBoardFilterID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SchedulingBoardFilter", objectId = SchedulingBoardFilterID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MassPrintStudentScheduleRunHistories
	#'
	#' This function returns a dataframe or json object of MassPrintStudentScheduleRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MassPrintStudentScheduleRunHistory') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of MassPrintStudentScheduleRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMassPrintStudentScheduleRunHistories <- function(searchConditionsList = NULL, MassPrintStudentScheduleRunHistoryID = F, EntityID = F, SchoolYearID = F, RunDescription = F, MediaID = F, WorkflowInstanceID = F, RequestIdentifier = F, SendMessageOnComplete = F, StudentSelectType = F, ParameterData = F, ParameterDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, PrintLockerInfo = F, SortType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MassPrintStudentScheduleRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get MassPrintStudentScheduleRunHistory
	#'
	#' This function returns a dataframe or json object of a MassPrintStudentScheduleRunHistory
	#' @param MassPrintStudentScheduleRunHistoryID The ID of the MassPrintStudentScheduleRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MassPrintStudentScheduleRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getMassPrintStudentScheduleRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMassPrintStudentScheduleRunHistory <- function(MassPrintStudentScheduleRunHistoryID, EntityID = F, SchoolYearID = F, RunDescription = F, MediaID = F, WorkflowInstanceID = F, RequestIdentifier = F, SendMessageOnComplete = F, StudentSelectType = F, ParameterData = F, ParameterDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, PrintLockerInfo = F, SortType = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MassPrintStudentScheduleRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "MassPrintStudentScheduleRunHistory", objectId = MassPrintStudentScheduleRunHistoryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchedulingTempExceptions
	#'
	#' This function returns a dataframe or json object of SchedulingTempExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingTempException') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SchedulingTempExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchedulingTempExceptions <- function(searchConditionsList = NULL, TempExceptionID = F, FailedRecordPrimaryKey = F, ErrorFieldName = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SchedulingTempException
	#'
	#' This function returns a dataframe or json object of a SchedulingTempException
	#' @param SchedulingTempExceptionID The ID of the SchedulingTempException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SchedulingTempException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSchedulingTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchedulingTempException <- function(SchedulingTempExceptionID, TempExceptionID = F, FailedRecordPrimaryKey = F, ErrorFieldName = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchedulingTempExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempException", objectId = SchedulingTempExceptionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMeets
	#'
	#' This function returns a dataframe or json object of TempMeets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempMeet') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempMeets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMeets <- function(searchConditionsList = NULL, TempMeetID = F, MeetID = F, CourseCode = F, SectionCode = F, CourseDescription = F, StartDate = F, EndDate = F, PrimaryDisplayPeriod = F, PrimaryDays = F, PrimaryStaffFullNameFML = F, RoomNumberDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionID = F, SectionLengthCode = F, NewSectionLengthCode = F, NewStartDate = F, NewEndDate = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempMeet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempMeet
	#'
	#' This function returns a dataframe or json object of a TempMeet
	#' @param TempMeetID The ID of the TempMeet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempMeet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMeet <- function(TempMeetID, MeetID = F, CourseCode = F, SectionCode = F, CourseDescription = F, StartDate = F, EndDate = F, PrimaryDisplayPeriod = F, PrimaryDays = F, PrimaryStaffFullNameFML = F, RoomNumberDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionID = F, SectionLengthCode = F, NewSectionLengthCode = F, NewStartDate = F, NewEndDate = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMeetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempMeet", objectId = TempMeetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSchedulerProposedMeetDisplayPeriods
	#'
	#' This function returns a dataframe or json object of SectionSchedulerProposedMeetDisplayPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerProposedMeetDisplayPeriod') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionSchedulerProposedMeetDisplayPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSchedulerProposedMeetDisplayPeriods <- function(searchConditionsList = NULL, SectionSchedulerProposedMeetDisplayPeriodID = F, SectionSchedulerProposedMeetID = F, DisplayPeriodID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerProposedMeetDisplayPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSchedulerProposedMeetDisplayPeriod
	#'
	#' This function returns a dataframe or json object of a SectionSchedulerProposedMeetDisplayPeriod
	#' @param SectionSchedulerProposedMeetDisplayPeriodID The ID of the SectionSchedulerProposedMeetDisplayPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerProposedMeetDisplayPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionSchedulerProposedMeetDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSchedulerProposedMeetDisplayPeriod <- function(SectionSchedulerProposedMeetDisplayPeriodID, SectionSchedulerProposedMeetID = F, DisplayPeriodID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSchedulerProposedMeetDisplayPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionSchedulerProposedMeetDisplayPeriod", objectId = SectionSchedulerProposedMeetDisplayPeriodID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSchedulerProposedMeets
	#'
	#' This function returns a dataframe or json object of SectionSchedulerProposedMeets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerProposedMeet') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionSchedulerProposedMeets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSchedulerProposedMeets <- function(searchConditionsList = NULL, SectionSchedulerProposedMeetID = F, SectionSchedulerRunAnalysisID = F, DisplayPeriodRotationID = F, MeetDisplayPeriodSummary = F, NumberOfActualConflicts = F, NumberOfEstimatedConflicts = F, PrimaryStaffMeetFullNameLFM = F, Rank = F, RankValue = F, RoomID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalActualConflictsPointsEarned = F, TotalDisplayPeriodPointsEarned = F, TotalEstimatedConflictsPointsEarned = F, TotalRoomPointsEarned = F, TotalStaffPointsEarned = F, SumTotalOfMaximumStudentCountForScheduledSections = F, SumTotalOfOptimalStudentCountForScheduledSections = F, TotalSumOfMaximumStudentCountForScheduledSectionsPointsEarned = F, TotalSumOfOptimalStudentCountForScheduledSectionsPointsEarned = F, NumberOfProposedMeetCourseConflicts = F, NumberOfProposedMeetRoomConflicts = F, NumberOfProposedMeetStaffConflicts = F, EndDate = F, SectionLengthID = F, StartDate = F, NumberOfProposedMeetRoomAndStaffConflicts = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerProposedMeet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSchedulerProposedMeet
	#'
	#' This function returns a dataframe or json object of a SectionSchedulerProposedMeet
	#' @param SectionSchedulerProposedMeetID The ID of the SectionSchedulerProposedMeet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerProposedMeet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionSchedulerProposedMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSchedulerProposedMeet <- function(SectionSchedulerProposedMeetID, SectionSchedulerRunAnalysisID = F, DisplayPeriodRotationID = F, MeetDisplayPeriodSummary = F, NumberOfActualConflicts = F, NumberOfEstimatedConflicts = F, PrimaryStaffMeetFullNameLFM = F, Rank = F, RankValue = F, RoomID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalActualConflictsPointsEarned = F, TotalDisplayPeriodPointsEarned = F, TotalEstimatedConflictsPointsEarned = F, TotalRoomPointsEarned = F, TotalStaffPointsEarned = F, SumTotalOfMaximumStudentCountForScheduledSections = F, SumTotalOfOptimalStudentCountForScheduledSections = F, TotalSumOfMaximumStudentCountForScheduledSectionsPointsEarned = F, TotalSumOfOptimalStudentCountForScheduledSectionsPointsEarned = F, NumberOfProposedMeetCourseConflicts = F, NumberOfProposedMeetRoomConflicts = F, NumberOfProposedMeetStaffConflicts = F, EndDate = F, SectionLengthID = F, StartDate = F, NumberOfProposedMeetRoomAndStaffConflicts = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSchedulerProposedMeetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionSchedulerProposedMeet", objectId = SectionSchedulerProposedMeetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSchedulerRunAnalyses
	#'
	#' This function returns a dataframe or json object of SectionSchedulerRunAnalyses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerRunAnalysis') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionSchedulerRunAnalyses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSchedulerRunAnalyses <- function(searchConditionsList = NULL, SectionSchedulerRunAnalysisID = F, EntityID = F, SchoolYearID = F, MeetID = F, PageStateID = F, UserIDPerformer = F, AnalysisMethod = F, StartTimeAnalysis = F, EndTimeAnalysis = F, AcceptedMeetReverted = F, SectionSchedulerProposedMeetIDAccepted = F, AnalysisDuration = F, CountOfSectionSchedulerProposedMeetRecords = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalActualConflictsPointsPossible = F, TotalDisplayPeriodPointsPossible = F, TotalEstimatedConflictsPointsPossible = F, TotalRoomPointsPossible = F, TotalStaffPointsPossible = F, TotalSumOfMaximumStudentCountForScheduledSectionsPointsPossible = F, TotalSumOfOptimalStudentCountForScheduledSectionsPointsPossible = F, AnalyzeMeetDisplayPeriods = F, AnalyzeRoom = F, AnalyzeSectionLength = F, AnalyzeStaffMeets = F, ExcludeMeetsPreviouslyScheduled = F, RunReason = F, AnalyzeDayRotations = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerRunAnalysis", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSchedulerRunAnalysis
	#'
	#' This function returns a dataframe or json object of a SectionSchedulerRunAnalysis
	#' @param SectionSchedulerRunAnalysisID The ID of the SectionSchedulerRunAnalysis to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerRunAnalysis') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionSchedulerRunAnalysis
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSchedulerRunAnalysis <- function(SectionSchedulerRunAnalysisID, EntityID = F, SchoolYearID = F, MeetID = F, PageStateID = F, UserIDPerformer = F, AnalysisMethod = F, StartTimeAnalysis = F, EndTimeAnalysis = F, AcceptedMeetReverted = F, SectionSchedulerProposedMeetIDAccepted = F, AnalysisDuration = F, CountOfSectionSchedulerProposedMeetRecords = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalActualConflictsPointsPossible = F, TotalDisplayPeriodPointsPossible = F, TotalEstimatedConflictsPointsPossible = F, TotalRoomPointsPossible = F, TotalStaffPointsPossible = F, TotalSumOfMaximumStudentCountForScheduledSectionsPointsPossible = F, TotalSumOfOptimalStudentCountForScheduledSectionsPointsPossible = F, AnalyzeMeetDisplayPeriods = F, AnalyzeRoom = F, AnalyzeSectionLength = F, AnalyzeStaffMeets = F, ExcludeMeetsPreviouslyScheduled = F, RunReason = F, AnalyzeDayRotations = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSchedulerRunAnalysisID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionSchedulerRunAnalysis", objectId = SectionSchedulerRunAnalysisID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSchedulerRoomTypeForCourses
	#'
	#' This function returns a dataframe or json object of SectionSchedulerRoomTypeForCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerRoomTypeForCourse') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionSchedulerRoomTypeForCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSchedulerRoomTypeForCourses <- function(searchConditionsList = NULL, SectionSchedulerRoomTypeForCourseID = F, CourseID = F, RoomTypeID = F, SectionSchedulerRoomTypeForCourseIDClonedFrom = F, SectionSchedulerRoomTypeForCourseIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerRoomTypeForCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSchedulerRoomTypeForCourse
	#'
	#' This function returns a dataframe or json object of a SectionSchedulerRoomTypeForCourse
	#' @param SectionSchedulerRoomTypeForCourseID The ID of the SectionSchedulerRoomTypeForCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerRoomTypeForCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionSchedulerRoomTypeForCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSchedulerRoomTypeForCourse <- function(SectionSchedulerRoomTypeForCourseID, CourseID = F, RoomTypeID = F, SectionSchedulerRoomTypeForCourseIDClonedFrom = F, SectionSchedulerRoomTypeForCourseIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSchedulerRoomTypeForCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionSchedulerRoomTypeForCourse", objectId = SectionSchedulerRoomTypeForCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSchedulerStaffForCourses
	#'
	#' This function returns a dataframe or json object of SectionSchedulerStaffForCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerStaffForCourse') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionSchedulerStaffForCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSchedulerStaffForCourses <- function(searchConditionsList = NULL, SectionSchedulerStaffForCourseID = F, CourseID = F, StaffID = F, SectionSchedulerStaffForCourseIDClonedFrom = F, SectionSchedulerStaffForCourseIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerStaffForCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSchedulerStaffForCourse
	#'
	#' This function returns a dataframe or json object of a SectionSchedulerStaffForCourse
	#' @param SectionSchedulerStaffForCourseID The ID of the SectionSchedulerStaffForCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerStaffForCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionSchedulerStaffForCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSchedulerStaffForCourse <- function(SectionSchedulerStaffForCourseID, CourseID = F, StaffID = F, SectionSchedulerStaffForCourseIDClonedFrom = F, SectionSchedulerStaffForCourseIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSchedulerStaffForCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionSchedulerStaffForCourse", objectId = SectionSchedulerStaffForCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSchedulerProposedMeetConflicts
	#'
	#' This function returns a dataframe or json object of SectionSchedulerProposedMeetConflicts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerProposedMeetConflict') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionSchedulerProposedMeetConflicts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSchedulerProposedMeetConflicts <- function(searchConditionsList = NULL, SectionSchedulerProposedMeetConflictID = F, SectionSchedulerProposedMeetID = F, ConflictType = F, Description = F, Name = F, Severity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, NumberOfActualConflicts = F, NumberOfCommonRequests = F, NumberOfEstimatedConflicts = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerProposedMeetConflict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSchedulerProposedMeetConflict
	#'
	#' This function returns a dataframe or json object of a SectionSchedulerProposedMeetConflict
	#' @param SectionSchedulerProposedMeetConflictID The ID of the SectionSchedulerProposedMeetConflict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerProposedMeetConflict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionSchedulerProposedMeetConflict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSchedulerProposedMeetConflict <- function(SectionSchedulerProposedMeetConflictID, SectionSchedulerProposedMeetID = F, ConflictType = F, Description = F, Name = F, Severity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, NumberOfActualConflicts = F, NumberOfCommonRequests = F, NumberOfEstimatedConflicts = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSchedulerProposedMeetConflictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionSchedulerProposedMeetConflict", objectId = SectionSchedulerProposedMeetConflictID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFailedStudentCourseRequestToReactivateDetails
	#'
	#' This function returns a dataframe or json object of TempFailedStudentCourseRequestToReactivateDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedStudentCourseRequestToReactivateDetail') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempFailedStudentCourseRequestToReactivateDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFailedStudentCourseRequestToReactivateDetails <- function(searchConditionsList = NULL, TempFailedStudentCourseRequestToReactivateDetailID = F, TempRecordToReactivatePrimaryKeyValue = F, Note = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedStudentCourseRequestToReactivateDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempFailedStudentCourseRequestToReactivateDetail
	#'
	#' This function returns a dataframe or json object of a TempFailedStudentCourseRequestToReactivateDetail
	#' @param TempFailedStudentCourseRequestToReactivateDetailID The ID of the TempFailedStudentCourseRequestToReactivateDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedStudentCourseRequestToReactivateDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempFailedStudentCourseRequestToReactivateDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFailedStudentCourseRequestToReactivateDetail <- function(TempFailedStudentCourseRequestToReactivateDetailID, TempRecordToReactivatePrimaryKeyValue = F, Note = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFailedStudentCourseRequestToReactivateDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempFailedStudentCourseRequestToReactivateDetail", objectId = TempFailedStudentCourseRequestToReactivateDetailID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFailedStudentCourseRequestToReactivates
	#'
	#' This function returns a dataframe or json object of TempFailedStudentCourseRequestToReactivates
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedStudentCourseRequestToReactivate') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempFailedStudentCourseRequestToReactivates
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFailedStudentCourseRequestToReactivates <- function(searchConditionsList = NULL, TempFailedStudentCourseRequestToReactivateID = F, TempRecordToReactivatePrimaryKeyValue = F, Note = F, StudentID = F, DateFrom = F, DateTo = F, StudentCourseRequestID = F, IsAlternate = F, AlternateRank = F, StudentCourseRequestIDAlternateFor = F, RequestStatus = F, RequestSource = F, SchedulingMethod = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, CourseCode = F, CourseDescription = F, CourseCodeDescription = F, EarnedCredits = F, CourseGradeLevelSummary = F, AuditRecordIsSchedulable = F, SectionID = F, SectionLengthID = F, ExcludeFromReportCardsAndTranscripts = F, IsTransferCourse = F, UseEarnedCreditOverride = F, EarnedCreditOverride = F, UseEarnedCreditTotalOverride = F, TotalEarnedCreditOverride = F, TotalFailedCreditOverride = F, GradeReferenceID = F, TransferCourseName = F, SectionCode = F, CurrentEnrollment = F, MaximumStudentCount = F, Period = F, Days = F, StaffFullNameFML = F, ExcludeFromStudentSectionLink = F, EntityIDCourse = F, SchoolYearIDCourse = F, StartDate = F, EndDate = F, SectionLengthSubsetID = F, EarlyExitReasonID = F, EarlyExitReasonCodeDescription = F, NameIDRequestedBy = F, NameRequestedByLFM = F, EntityIDCountsAs = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PreventReactivateCheckboxFromBeingRendered = F, AuditRecordIsRequestable = F, StudentSectionID = F, StudentSectionTransactionID = F, SectionLengthSubsetCode = F, SectionLengthSubsetDescription = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedStudentCourseRequestToReactivate", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempFailedStudentCourseRequestToReactivate
	#'
	#' This function returns a dataframe or json object of a TempFailedStudentCourseRequestToReactivate
	#' @param TempFailedStudentCourseRequestToReactivateID The ID of the TempFailedStudentCourseRequestToReactivate to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedStudentCourseRequestToReactivate') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempFailedStudentCourseRequestToReactivate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFailedStudentCourseRequestToReactivate <- function(TempFailedStudentCourseRequestToReactivateID, TempRecordToReactivatePrimaryKeyValue = F, Note = F, StudentID = F, DateFrom = F, DateTo = F, StudentCourseRequestID = F, IsAlternate = F, AlternateRank = F, StudentCourseRequestIDAlternateFor = F, RequestStatus = F, RequestSource = F, SchedulingMethod = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, CourseCode = F, CourseDescription = F, CourseCodeDescription = F, EarnedCredits = F, CourseGradeLevelSummary = F, AuditRecordIsSchedulable = F, SectionID = F, SectionLengthID = F, ExcludeFromReportCardsAndTranscripts = F, IsTransferCourse = F, UseEarnedCreditOverride = F, EarnedCreditOverride = F, UseEarnedCreditTotalOverride = F, TotalEarnedCreditOverride = F, TotalFailedCreditOverride = F, GradeReferenceID = F, TransferCourseName = F, SectionCode = F, CurrentEnrollment = F, MaximumStudentCount = F, Period = F, Days = F, StaffFullNameFML = F, ExcludeFromStudentSectionLink = F, EntityIDCourse = F, SchoolYearIDCourse = F, StartDate = F, EndDate = F, SectionLengthSubsetID = F, EarlyExitReasonID = F, EarlyExitReasonCodeDescription = F, NameIDRequestedBy = F, NameRequestedByLFM = F, EntityIDCountsAs = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PreventReactivateCheckboxFromBeingRendered = F, AuditRecordIsRequestable = F, StudentSectionID = F, StudentSectionTransactionID = F, SectionLengthSubsetCode = F, SectionLengthSubsetDescription = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFailedStudentCourseRequestToReactivateID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempFailedStudentCourseRequestToReactivate", objectId = TempFailedStudentCourseRequestToReactivateID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentCourseRequestToReactivateMNS
	#'
	#' This function returns a dataframe or json object of TempStudentCourseRequestToReactivateMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentCourseRequestToReactivateMN') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempStudentCourseRequestToReactivateMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentCourseRequestToReactivateMNS <- function(searchConditionsList = NULL, TempStudentCourseRequestToReactivateMNID = F, IsTSAProficient = F, StudentID = F, DateFrom = F, DateTo = F, StudentCourseRequestID = F, IsAlternate = F, AlternateRank = F, StudentCourseRequestIDAlternateFor = F, RequestStatus = F, RequestSource = F, SchedulingMethod = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, CourseCode = F, CourseDescription = F, CourseCodeDescription = F, EarnedCredits = F, CourseGradeLevelSummary = F, AuditRecordIsSchedulable = F, SectionID = F, SectionLengthID = F, ExcludeFromReportCardsAndTranscripts = F, IsTransferCourse = F, UseEarnedCreditOverride = F, EarnedCreditOverride = F, UseEarnedCreditTotalOverride = F, TotalEarnedCreditOverride = F, TotalFailedCreditOverride = F, GradeReferenceID = F, TransferCourseName = F, SectionCode = F, CurrentEnrollment = F, MaximumStudentCount = F, Period = F, Days = F, StaffFullNameFML = F, ExcludeFromStudentSectionLink = F, EntityIDCourse = F, SchoolYearIDCourse = F, StartDate = F, EndDate = F, SectionLengthSubsetID = F, EarlyExitReasonID = F, EarlyExitReasonCodeDescription = F, NameIDRequestedBy = F, NameRequestedByLFM = F, EntityIDCountsAs = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PreventReactivateCheckboxFromBeingRendered = F, AuditRecordIsRequestable = F, StudentSectionID = F, StudentSectionTransactionID = F, SectionLengthSubsetCode = F, SectionLengthSubsetDescription = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentCourseRequestToReactivateMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempStudentCourseRequestToReactivateMN
	#'
	#' This function returns a dataframe or json object of a TempStudentCourseRequestToReactivateMN
	#' @param TempStudentCourseRequestToReactivateMNID The ID of the TempStudentCourseRequestToReactivateMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentCourseRequestToReactivateMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempStudentCourseRequestToReactivateMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentCourseRequestToReactivateMN <- function(TempStudentCourseRequestToReactivateMNID, IsTSAProficient = F, StudentID = F, DateFrom = F, DateTo = F, StudentCourseRequestID = F, IsAlternate = F, AlternateRank = F, StudentCourseRequestIDAlternateFor = F, RequestStatus = F, RequestSource = F, SchedulingMethod = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, CourseCode = F, CourseDescription = F, CourseCodeDescription = F, EarnedCredits = F, CourseGradeLevelSummary = F, AuditRecordIsSchedulable = F, SectionID = F, SectionLengthID = F, ExcludeFromReportCardsAndTranscripts = F, IsTransferCourse = F, UseEarnedCreditOverride = F, EarnedCreditOverride = F, UseEarnedCreditTotalOverride = F, TotalEarnedCreditOverride = F, TotalFailedCreditOverride = F, GradeReferenceID = F, TransferCourseName = F, SectionCode = F, CurrentEnrollment = F, MaximumStudentCount = F, Period = F, Days = F, StaffFullNameFML = F, ExcludeFromStudentSectionLink = F, EntityIDCourse = F, SchoolYearIDCourse = F, StartDate = F, EndDate = F, SectionLengthSubsetID = F, EarlyExitReasonID = F, EarlyExitReasonCodeDescription = F, NameIDRequestedBy = F, NameRequestedByLFM = F, EntityIDCountsAs = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PreventReactivateCheckboxFromBeingRendered = F, AuditRecordIsRequestable = F, StudentSectionID = F, StudentSectionTransactionID = F, SectionLengthSubsetCode = F, SectionLengthSubsetDescription = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentCourseRequestToReactivateMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempStudentCourseRequestToReactivateMN", objectId = TempStudentCourseRequestToReactivateMNID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSchedulerPatternExcludedForMeetRequirements
	#'
	#' This function returns a dataframe or json object of SectionSchedulerPatternExcludedForMeetRequirements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerPatternExcludedForMeetRequirement') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionSchedulerPatternExcludedForMeetRequirements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSchedulerPatternExcludedForMeetRequirements <- function(searchConditionsList = NULL, SectionSchedulerPatternExcludedForMeetRequirementID = F, MeetRequirementID = F, SectionSchedulerPatternID = F, SectionSchedulerPatternExcludedForMeetRequirementIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerPatternExcludedForMeetRequirement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSchedulerPatternExcludedForMeetRequirement
	#'
	#' This function returns a dataframe or json object of a SectionSchedulerPatternExcludedForMeetRequirement
	#' @param SectionSchedulerPatternExcludedForMeetRequirementID The ID of the SectionSchedulerPatternExcludedForMeetRequirement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerPatternExcludedForMeetRequirement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionSchedulerPatternExcludedForMeetRequirement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSchedulerPatternExcludedForMeetRequirement <- function(SectionSchedulerPatternExcludedForMeetRequirementID, MeetRequirementID = F, SectionSchedulerPatternID = F, SectionSchedulerPatternExcludedForMeetRequirementIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSchedulerPatternExcludedForMeetRequirementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionSchedulerPatternExcludedForMeetRequirement", objectId = SectionSchedulerPatternExcludedForMeetRequirementID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSchedulerPatternDayRotations
	#'
	#' This function returns a dataframe or json object of SectionSchedulerPatternDayRotations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerPatternDayRotation') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionSchedulerPatternDayRotations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSchedulerPatternDayRotations <- function(searchConditionsList = NULL, SectionSchedulerPatternDayRotationID = F, SectionSchedulerPatternID = F, DayRotationID = F, SectionSchedulerPatternDayRotationIDClonedFrom = F, SectionSchedulerPatternDayRotationClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerPatternDayRotation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSchedulerPatternDayRotation
	#'
	#' This function returns a dataframe or json object of a SectionSchedulerPatternDayRotation
	#' @param SectionSchedulerPatternDayRotationID The ID of the SectionSchedulerPatternDayRotation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerPatternDayRotation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionSchedulerPatternDayRotation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSchedulerPatternDayRotation <- function(SectionSchedulerPatternDayRotationID, SectionSchedulerPatternID = F, DayRotationID = F, SectionSchedulerPatternDayRotationIDClonedFrom = F, SectionSchedulerPatternDayRotationClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSchedulerPatternDayRotationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionSchedulerPatternDayRotation", objectId = SectionSchedulerPatternDayRotationID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSchedulerPatterns
	#'
	#' This function returns a dataframe or json object of SectionSchedulerPatterns
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerPattern') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionSchedulerPatterns
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSchedulerPatterns <- function(searchConditionsList = NULL, SectionSchedulerPatternID = F, EntityID = F, SchoolYearID = F, Code = F, Description = F, DayRotationSummary = F, SectionSchedulerPatternIDClonedFrom = F, CodeDescription = F, SectionSchedulerPatternIDClonedTo = F, HasDayRotations = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DayRotationCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerPattern", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSchedulerPattern
	#'
	#' This function returns a dataframe or json object of a SectionSchedulerPattern
	#' @param SectionSchedulerPatternID The ID of the SectionSchedulerPattern to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerPattern') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionSchedulerPattern
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSchedulerPattern <- function(SectionSchedulerPatternID, EntityID = F, SchoolYearID = F, Code = F, Description = F, DayRotationSummary = F, SectionSchedulerPatternIDClonedFrom = F, CodeDescription = F, SectionSchedulerPatternIDClonedTo = F, HasDayRotations = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DayRotationCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSchedulerPatternID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionSchedulerPattern", objectId = SectionSchedulerPatternID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseMasterMassUpdates
	#'
	#' This function returns a dataframe or json object of CourseMasterMassUpdates
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseMasterMassUpdate') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseMasterMassUpdates
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseMasterMassUpdates <- function(searchConditionsList = NULL, CourseMasterMassUpdateID = F, UserIDRanBy = F, EntityID = F, SchoolYearID = F, RunReason = F, FilterParameters = F, ValueParameters = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseMasterMassUpdate", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseMasterMassUpdate
	#'
	#' This function returns a dataframe or json object of a CourseMasterMassUpdate
	#' @param CourseMasterMassUpdateID The ID of the CourseMasterMassUpdate to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseMasterMassUpdate') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseMasterMassUpdate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseMasterMassUpdate <- function(CourseMasterMassUpdateID, UserIDRanBy = F, EntityID = F, SchoolYearID = F, RunReason = F, FilterParameters = F, ValueParameters = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseMasterMassUpdateID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseMasterMassUpdate", objectId = CourseMasterMassUpdateID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCourseMasterMassUpdateErrors
	#'
	#' This function returns a dataframe or json object of TempCourseMasterMassUpdateErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourseMasterMassUpdateError') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempCourseMasterMassUpdateErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCourseMasterMassUpdateErrors <- function(searchConditionsList = NULL, TempCourseMasterMassUpdateErrorID = F, SourceDescription = F, BaseTableName = F, TableName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempCourseMasterMassUpdateError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempCourseMasterMassUpdateError
	#'
	#' This function returns a dataframe or json object of a TempCourseMasterMassUpdateError
	#' @param TempCourseMasterMassUpdateErrorID The ID of the TempCourseMasterMassUpdateError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourseMasterMassUpdateError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempCourseMasterMassUpdateError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCourseMasterMassUpdateError <- function(TempCourseMasterMassUpdateErrorID, SourceDescription = F, BaseTableName = F, TableName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCourseMasterMassUpdateErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempCourseMasterMassUpdateError", objectId = TempCourseMasterMassUpdateErrorID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCourseMasterMassUpdateFields
	#'
	#' This function returns a dataframe or json object of TempCourseMasterMassUpdateFields
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourseMasterMassUpdateField') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempCourseMasterMassUpdateFields
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCourseMasterMassUpdateFields <- function(searchConditionsList = NULL, TempCourseMasterMassUpdateFieldID = F, CourseID = F, AffectedPrimaryKey = F, SourceDescription = F, FieldName = F, OriginalValue = F, FriendlyOriginalValue = F, UpdatedValue = F, FriendlyUpdatedValue = F, TableName = F, BaseTableName = F, UpdateRank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FieldDisplayName = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempCourseMasterMassUpdateField", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempCourseMasterMassUpdateField
	#'
	#' This function returns a dataframe or json object of a TempCourseMasterMassUpdateField
	#' @param TempCourseMasterMassUpdateFieldID The ID of the TempCourseMasterMassUpdateField to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourseMasterMassUpdateField') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempCourseMasterMassUpdateField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCourseMasterMassUpdateField <- function(TempCourseMasterMassUpdateFieldID, CourseID = F, AffectedPrimaryKey = F, SourceDescription = F, FieldName = F, OriginalValue = F, FriendlyOriginalValue = F, UpdatedValue = F, FriendlyUpdatedValue = F, TableName = F, BaseTableName = F, UpdateRank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FieldDisplayName = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCourseMasterMassUpdateFieldID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempCourseMasterMassUpdateField", objectId = TempCourseMasterMassUpdateFieldID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSchedulerProposedStaffMeets
	#'
	#' This function returns a dataframe or json object of SectionSchedulerProposedStaffMeets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerProposedStaffMeet') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionSchedulerProposedStaffMeets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSchedulerProposedStaffMeets <- function(searchConditionsList = NULL, SectionSchedulerProposedStaffMeetID = F, SectionSchedulerProposedMeetID = F, StaffID = F, EffectiveStartDate = F, EffectiveEndDate = F, IsPrimary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerProposedStaffMeet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSchedulerProposedStaffMeet
	#'
	#' This function returns a dataframe or json object of a SectionSchedulerProposedStaffMeet
	#' @param SectionSchedulerProposedStaffMeetID The ID of the SectionSchedulerProposedStaffMeet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerProposedStaffMeet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionSchedulerProposedStaffMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSchedulerProposedStaffMeet <- function(SectionSchedulerProposedStaffMeetID, SectionSchedulerProposedMeetID = F, StaffID = F, EffectiveStartDate = F, EffectiveEndDate = F, IsPrimary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSchedulerProposedStaffMeetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionSchedulerProposedStaffMeet", objectId = SectionSchedulerProposedStaffMeetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StaffStudents
	#'
	#' This function returns a dataframe or json object of StaffStudents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StaffStudent') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StaffStudents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStaffStudents <- function(searchConditionsList = NULL, StudentID = F, StaffID = F, EntityID = F, SchoolYearID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StaffStudent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StaffStudent
	#'
	#' This function returns a dataframe or json object of a StaffStudent
	#' @param StaffStudentID The ID of the StaffStudent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StaffStudent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStaffStudent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStaffStudent <- function(StaffStudentID, StudentID = F, StaffID = F, EntityID = F, SchoolYearID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StaffStudentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StaffStudent", objectId = StaffStudentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCourseMasterMassUpdateErrorDetails
	#'
	#' This function returns a dataframe or json object of TempCourseMasterMassUpdateErrorDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourseMasterMassUpdateErrorDetail') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempCourseMasterMassUpdateErrorDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCourseMasterMassUpdateErrorDetails <- function(searchConditionsList = NULL, TempCourseMasterMassUpdateErrorDetailID = F, TempCourseMasterMassUpdateErrorID = F, ErrorName = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempCourseMasterMassUpdateErrorDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempCourseMasterMassUpdateErrorDetail
	#'
	#' This function returns a dataframe or json object of a TempCourseMasterMassUpdateErrorDetail
	#' @param TempCourseMasterMassUpdateErrorDetailID The ID of the TempCourseMasterMassUpdateErrorDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempCourseMasterMassUpdateErrorDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempCourseMasterMassUpdateErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCourseMasterMassUpdateErrorDetail <- function(TempCourseMasterMassUpdateErrorDetailID, TempCourseMasterMassUpdateErrorID = F, ErrorName = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCourseMasterMassUpdateErrorDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempCourseMasterMassUpdateErrorDetail", objectId = TempCourseMasterMassUpdateErrorDetailID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionEnrollmentTotalForSectionLengthSubsets
	#'
	#' This function returns a dataframe or json object of SectionEnrollmentTotalForSectionLengthSubsets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionEnrollmentTotalForSectionLengthSubset') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionEnrollmentTotalForSectionLengthSubsets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionEnrollmentTotalForSectionLengthSubsets <- function(searchConditionsList = NULL, SectionID = F, SectionLengthSubsetID = F, TotalEnrollmentCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionEnrollmentTotalForSectionLengthSubset", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionEnrollmentTotalForSectionLengthSubset
	#'
	#' This function returns a dataframe or json object of a SectionEnrollmentTotalForSectionLengthSubset
	#' @param SectionEnrollmentTotalForSectionLengthSubsetID The ID of the SectionEnrollmentTotalForSectionLengthSubset to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionEnrollmentTotalForSectionLengthSubset') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionEnrollmentTotalForSectionLengthSubset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionEnrollmentTotalForSectionLengthSubset <- function(SectionEnrollmentTotalForSectionLengthSubsetID, SectionID = F, SectionLengthSubsetID = F, TotalEnrollmentCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionEnrollmentTotalForSectionLengthSubsetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionEnrollmentTotalForSectionLengthSubset", objectId = SectionEnrollmentTotalForSectionLengthSubsetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStaffMeets
	#'
	#' This function returns a dataframe or json object of TempStaffMeets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStaffMeet') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempStaffMeets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStaffMeets <- function(searchConditionsList = NULL, TempStaffMeetID = F, StaffMeetID = F, MeetID = F, SectionID = F, CourseCode = F, SectionCode = F, CourseDescription = F, EffectiveStartDate = F, EffectiveEndDate = F, IsPrimary = F, IsSubstitute = F, StaffID = F, StaffFullNameFML = F, NewEffectiveStartDate = F, NewEffectiveEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsLongTermSubstitute = F, HasAttendanceAccess = F, HasGradebookAccess = F, IsChecked = F, NewStaffID = F, NewStaffFullNameFML = F, WorkflowAction = F, Note = F, Conflicts = F, HasConflicts = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStaffMeet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempStaffMeet
	#'
	#' This function returns a dataframe or json object of a TempStaffMeet
	#' @param TempStaffMeetID The ID of the TempStaffMeet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStaffMeet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempStaffMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStaffMeet <- function(TempStaffMeetID, StaffMeetID = F, MeetID = F, SectionID = F, CourseCode = F, SectionCode = F, CourseDescription = F, EffectiveStartDate = F, EffectiveEndDate = F, IsPrimary = F, IsSubstitute = F, StaffID = F, StaffFullNameFML = F, NewEffectiveStartDate = F, NewEffectiveEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsLongTermSubstitute = F, HasAttendanceAccess = F, HasGradebookAccess = F, IsChecked = F, NewStaffID = F, NewStaffFullNameFML = F, WorkflowAction = F, Note = F, Conflicts = F, HasConflicts = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStaffMeetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempStaffMeet", objectId = TempStaffMeetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionEnrollmentTotalForSectionLengthSubsetAndEntities
	#'
	#' This function returns a dataframe or json object of SectionEnrollmentTotalForSectionLengthSubsetAndEntities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionEnrollmentTotalForSectionLengthSubsetAndEntity') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionEnrollmentTotalForSectionLengthSubsetAndEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionEnrollmentTotalForSectionLengthSubsetAndEntities <- function(searchConditionsList = NULL, SectionID = F, EntityIDCountsAs = F, SectionLengthSubsetID = F, TotalEnrollmentCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionEnrollmentTotalForSectionLengthSubsetAndEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionEnrollmentTotalForSectionLengthSubsetAndEntity
	#'
	#' This function returns a dataframe or json object of a SectionEnrollmentTotalForSectionLengthSubsetAndEntity
	#' @param SectionEnrollmentTotalForSectionLengthSubsetAndEntityID The ID of the SectionEnrollmentTotalForSectionLengthSubsetAndEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionEnrollmentTotalForSectionLengthSubsetAndEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionEnrollmentTotalForSectionLengthSubsetAndEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionEnrollmentTotalForSectionLengthSubsetAndEntity <- function(SectionEnrollmentTotalForSectionLengthSubsetAndEntityID, SectionID = F, EntityIDCountsAs = F, SectionLengthSubsetID = F, TotalEnrollmentCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionEnrollmentTotalForSectionLengthSubsetAndEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionEnrollmentTotalForSectionLengthSubsetAndEntity", objectId = SectionEnrollmentTotalForSectionLengthSubsetAndEntityID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionMeetSummaries
	#'
	#' This function returns a dataframe or json object of SectionMeetSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionMeetSummary') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionMeetSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionMeetSummaries <- function(searchConditionsList = NULL, SectionMeetSummaryID = F, SectionID = F, EntityIDViewing = F, CalendarID = F, IsDefaultCalendar = F, PeriodDaySummary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionMeetSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionMeetSummary
	#'
	#' This function returns a dataframe or json object of a SectionMeetSummary
	#' @param SectionMeetSummaryID The ID of the SectionMeetSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionMeetSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionMeetSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionMeetSummary <- function(SectionMeetSummaryID, SectionID = F, EntityIDViewing = F, CalendarID = F, IsDefaultCalendar = F, PeriodDaySummary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionMeetSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionMeetSummary", objectId = SectionMeetSummaryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudents
	#'
	#' This function returns a dataframe or json object of TempStudents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudent') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempStudents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudents <- function(searchConditionsList = NULL, TempStudentID = F, StudentID = F, FullNameLFM = F, StudentNumber = F, GradeReferenceID = F, GradeLevelCode = F, GradYear = F, StudentTypeCodeDescription = F, CurrentActive = F, GenderCode = F, SchoolName = F, HomeRoomCode = F, IsSelected = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasConflictedStudentCourseRequest = F, CourseRequestCount = F, StudentSectionCount = F, HasFailedUpdate = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempStudent
	#'
	#' This function returns a dataframe or json object of a TempStudent
	#' @param TempStudentID The ID of the TempStudent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempStudent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudent <- function(TempStudentID, StudentID = F, FullNameLFM = F, StudentNumber = F, GradeReferenceID = F, GradeLevelCode = F, GradYear = F, StudentTypeCodeDescription = F, CurrentActive = F, GenderCode = F, SchoolName = F, HomeRoomCode = F, IsSelected = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasConflictedStudentCourseRequest = F, CourseRequestCount = F, StudentSectionCount = F, HasFailedUpdate = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempStudent", objectId = TempStudentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SectionSchedulerDayRotationForMeets
	#'
	#' This function returns a dataframe or json object of SectionSchedulerDayRotationForMeets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerDayRotationForMeet') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of SectionSchedulerDayRotationForMeets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSectionSchedulerDayRotationForMeets <- function(searchConditionsList = NULL, SectionSchedulerDayRotationForMeetID = F, MeetID = F, DayRotationID = F, SectionSchedulerDayRotationForMeetIDClonedFrom = F, SectionSchedulerDayRotationForMeetClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerDayRotationForMeet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get SectionSchedulerDayRotationForMeet
	#'
	#' This function returns a dataframe or json object of a SectionSchedulerDayRotationForMeet
	#' @param SectionSchedulerDayRotationForMeetID The ID of the SectionSchedulerDayRotationForMeet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SectionSchedulerDayRotationForMeet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getSectionSchedulerDayRotationForMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSectionSchedulerDayRotationForMeet <- function(SectionSchedulerDayRotationForMeetID, MeetID = F, DayRotationID = F, SectionSchedulerDayRotationForMeetIDClonedFrom = F, SectionSchedulerDayRotationForMeetClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SectionSchedulerDayRotationForMeetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "SectionSchedulerDayRotationForMeet", objectId = SectionSchedulerDayRotationForMeetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AppleSchoolManagerConfigs
	#'
	#' This function returns a dataframe or json object of AppleSchoolManagerConfigs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AppleSchoolManagerConfig') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of AppleSchoolManagerConfigs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAppleSchoolManagerConfigs <- function(searchConditionsList = NULL, AppleSchoolManagerConfigID = F, DistrictID = F, SchoolYearID = F, AsOfDateSetting = F, EntityIDSelectedList = F, SpecifiedAsOfDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ZipFileName = F, MediaID = F, FileDestinationIDSelectedList = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AppleSchoolManagerConfig", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get AppleSchoolManagerConfig
	#'
	#' This function returns a dataframe or json object of an AppleSchoolManagerConfig
	#' @param AppleSchoolManagerConfigID The ID of the AppleSchoolManagerConfig to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AppleSchoolManagerConfig') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getAppleSchoolManagerConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAppleSchoolManagerConfig <- function(AppleSchoolManagerConfigID, DistrictID = F, SchoolYearID = F, AsOfDateSetting = F, EntityIDSelectedList = F, SpecifiedAsOfDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ZipFileName = F, MediaID = F, FileDestinationIDSelectedList = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AppleSchoolManagerConfigID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "AppleSchoolManagerConfig", objectId = AppleSchoolManagerConfigID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFailedStaffMeets
	#'
	#' This function returns a dataframe or json object of TempFailedStaffMeets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedStaffMeet') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempFailedStaffMeets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFailedStaffMeets <- function(searchConditionsList = NULL, TempFailedStaffMeetID = F, TempStaffMeetID = F, ErrorCount = F, HasErrors = F, StaffMeetID = F, MeetID = F, SectionID = F, CourseCode = F, SectionCode = F, CourseDescription = F, EffectiveStartDate = F, EffectiveEndDate = F, HasAttendanceAccess = F, HasGradebookAccess = F, IsPrimary = F, IsChecked = F, IsSubstitute = F, IsLongTermSubstitute = F, StaffID = F, StaffFullNameFML = F, NewEffectiveStartDate = F, NewEffectiveEndDate = F, NewStaffID = F, NewStaffFullNameFML = F, WorkflowAction = F, Note = F, Conflicts = F, HasConflicts = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedStaffMeet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempFailedStaffMeet
	#'
	#' This function returns a dataframe or json object of a TempFailedStaffMeet
	#' @param TempFailedStaffMeetID The ID of the TempFailedStaffMeet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempFailedStaffMeet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempFailedStaffMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFailedStaffMeet <- function(TempFailedStaffMeetID, TempStaffMeetID = F, ErrorCount = F, HasErrors = F, StaffMeetID = F, MeetID = F, SectionID = F, CourseCode = F, SectionCode = F, CourseDescription = F, EffectiveStartDate = F, EffectiveEndDate = F, HasAttendanceAccess = F, HasGradebookAccess = F, IsPrimary = F, IsChecked = F, IsSubstitute = F, IsLongTermSubstitute = F, StaffID = F, StaffFullNameFML = F, NewEffectiveStartDate = F, NewEffectiveEndDate = F, NewStaffID = F, NewStaffFullNameFML = F, WorkflowAction = F, Note = F, Conflicts = F, HasConflicts = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFailedStaffMeetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempFailedStaffMeet", objectId = TempFailedStaffMeetID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List QueuedStudentSections
	#'
	#' This function returns a dataframe or json object of QueuedStudentSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('QueuedStudentSection') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of QueuedStudentSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listQueuedStudentSections <- function(searchConditionsList = NULL, QueuedStudentSectionID = F, StudentSectionID = F, IsComplete = F, SourceProcess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "QueuedStudentSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get QueuedStudentSection
	#'
	#' This function returns a dataframe or json object of a QueuedStudentSection
	#' @param QueuedStudentSectionID The ID of the QueuedStudentSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('QueuedStudentSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getQueuedStudentSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getQueuedStudentSection <- function(QueuedStudentSectionID, StudentSectionID = F, IsComplete = F, SourceProcess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "QueuedStudentSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "QueuedStudentSection", objectId = QueuedStudentSectionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentSectionTransactionRoomPeriods
	#'
	#' This function returns a dataframe or json object of StudentSectionTransactionRoomPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentSectionTransactionRoomPeriod') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of StudentSectionTransactionRoomPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentSectionTransactionRoomPeriods <- function(searchConditionsList = NULL, StudentSectionTransactionID = F, StudentSectionID = F, StudentID = F, MeetID = F, SectionID = F, StartDateMeet = F, EndDateMeet = F, StartDateStudentSectionTransaction = F, EndDateSectionSectionTransaction = F, EntityIDCountsAs = F, EntityIDCourse = F, EntityIDFor = F, EntityIDViewingCalculated = F, SchoolYearID = F, RoomID = F, UseRoomOverride = F, MeetSummaryID = F, EntityIDViewingMeetSummary = F, CalendarID = F, IsDefaultCalendar = F, DisplayPeriodID = F, SchedulingPeriodID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentSectionTransactionRoomPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get StudentSectionTransactionRoomPeriod
	#'
	#' This function returns a dataframe or json object of a StudentSectionTransactionRoomPeriod
	#' @param StudentSectionTransactionRoomPeriodID The ID of the StudentSectionTransactionRoomPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentSectionTransactionRoomPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getStudentSectionTransactionRoomPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentSectionTransactionRoomPeriod <- function(StudentSectionTransactionRoomPeriodID, StudentSectionTransactionID = F, StudentSectionID = F, StudentID = F, MeetID = F, SectionID = F, StartDateMeet = F, EndDateMeet = F, StartDateStudentSectionTransaction = F, EndDateSectionSectionTransaction = F, EntityIDCountsAs = F, EntityIDCourse = F, EntityIDFor = F, EntityIDViewingCalculated = F, SchoolYearID = F, RoomID = F, UseRoomOverride = F, MeetSummaryID = F, EntityIDViewingMeetSummary = F, CalendarID = F, IsDefaultCalendar = F, DisplayPeriodID = F, SchedulingPeriodID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentSectionTransactionRoomPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "StudentSectionTransactionRoomPeriod", objectId = StudentSectionTransactionRoomPeriodID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MeetEntityRoomPeriods
	#'
	#' This function returns a dataframe or json object of MeetEntityRoomPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MeetEntityRoomPeriod') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of MeetEntityRoomPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMeetEntityRoomPeriods <- function(searchConditionsList = NULL, MeetID = F, SectionID = F, StartDate = F, EndDate = F, EntityIDCourse = F, EntityIDFor = F, EntityIDViewingCalculated = F, SchoolYearID = F, RoomID = F, UseRoomOverride = F, MeetSummaryID = F, EntityIDViewingMeetSummary = F, CalendarID = F, IsDefaultCalendar = F, DisplayPeriodID = F, SchedulingPeriodID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MeetEntityRoomPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get MeetEntityRoomPeriod
	#'
	#' This function returns a dataframe or json object of a MeetEntityRoomPeriod
	#' @param MeetEntityRoomPeriodID The ID of the MeetEntityRoomPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MeetEntityRoomPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getMeetEntityRoomPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMeetEntityRoomPeriod <- function(MeetEntityRoomPeriodID, MeetID = F, SectionID = F, StartDate = F, EndDate = F, EntityIDCourse = F, EntityIDFor = F, EntityIDViewingCalculated = F, SchoolYearID = F, RoomID = F, UseRoomOverride = F, MeetSummaryID = F, EntityIDViewingMeetSummary = F, CalendarID = F, IsDefaultCalendar = F, DisplayPeriodID = F, SchedulingPeriodID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MeetEntityRoomPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "MeetEntityRoomPeriod", objectId = MeetEntityRoomPeriodID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MeetEntityRoomPeriodDateSeatsAvailables
	#'
	#' This function returns a dataframe or json object of MeetEntityRoomPeriodDateSeatsAvailables
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MeetEntityRoomPeriodDateSeatsAvailable') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of MeetEntityRoomPeriodDateSeatsAvailables
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMeetEntityRoomPeriodDateSeatsAvailables <- function(searchConditionsList = NULL, MeetID = F, SectionID = F, StartDate = F, EndDate = F, EntityIDCourse = F, EntityIDFor = F, EntityIDViewingCalculated = F, IsOfferedMeet = F, SchoolYearID = F, RoomIDMeet = F, RoomIDMeetOverride = F, RoomID = F, UseRoomOverride = F, MaxSeatsMeet = F, MaxSeatsMeetOverride = F, MaxSeats = F, MeetSummaryID = F, EntityIDViewingMeetSummary = F, CalendarID = F, DisplayPeriodID = F, SchedulingPeriodID = F, Date = F, RoomSeatsAvailable = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MeetEntityRoomPeriodDateSeatsAvailable", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get MeetEntityRoomPeriodDateSeatsAvailable
	#'
	#' This function returns a dataframe or json object of a MeetEntityRoomPeriodDateSeatsAvailable
	#' @param MeetEntityRoomPeriodDateSeatsAvailableID The ID of the MeetEntityRoomPeriodDateSeatsAvailable to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MeetEntityRoomPeriodDateSeatsAvailable') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getMeetEntityRoomPeriodDateSeatsAvailable
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMeetEntityRoomPeriodDateSeatsAvailable <- function(MeetEntityRoomPeriodDateSeatsAvailableID, MeetID = F, SectionID = F, StartDate = F, EndDate = F, EntityIDCourse = F, EntityIDFor = F, EntityIDViewingCalculated = F, IsOfferedMeet = F, SchoolYearID = F, RoomIDMeet = F, RoomIDMeetOverride = F, RoomID = F, UseRoomOverride = F, MaxSeatsMeet = F, MaxSeatsMeetOverride = F, MaxSeats = F, MeetSummaryID = F, EntityIDViewingMeetSummary = F, CalendarID = F, DisplayPeriodID = F, SchedulingPeriodID = F, Date = F, RoomSeatsAvailable = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MeetEntityRoomPeriodDateSeatsAvailableID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "MeetEntityRoomPeriodDateSeatsAvailable", objectId = MeetEntityRoomPeriodDateSeatsAvailableID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CoursePrerequisiteCurriculumCourseStudentCourseRequests
	#'
	#' This function returns a dataframe or json object of CoursePrerequisiteCurriculumCourseStudentCourseRequests
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CoursePrerequisiteCurriculumCourseStudentCourseRequest') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CoursePrerequisiteCurriculumCourseStudentCourseRequests
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCoursePrerequisiteCurriculumCourseStudentCourseRequests <- function(searchConditionsList = NULL, CoursePrerequisiteCurriculumCourseStudentCourseRequestID = F, PrerequisiteID = F, CurriculumIDFor = F, CurriculumIDRequired = F, CourseIDFor = F, StudentCourseRequestID = F, StudentSectionID = F, CourseIDRequired = F, StudentID = F, Status = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CoursePrerequisiteCurriculumCourseStudentCourseRequest", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CoursePrerequisiteCurriculumCourseStudentCourseRequest
	#'
	#' This function returns a dataframe or json object of a CoursePrerequisiteCurriculumCourseStudentCourseRequest
	#' @param CoursePrerequisiteCurriculumCourseStudentCourseRequestID The ID of the CoursePrerequisiteCurriculumCourseStudentCourseRequest to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CoursePrerequisiteCurriculumCourseStudentCourseRequest') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCoursePrerequisiteCurriculumCourseStudentCourseRequest
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCoursePrerequisiteCurriculumCourseStudentCourseRequest <- function(CoursePrerequisiteCurriculumCourseStudentCourseRequestID, PrerequisiteID = F, CurriculumIDFor = F, CurriculumIDRequired = F, CourseIDFor = F, StudentCourseRequestID = F, StudentSectionID = F, CourseIDRequired = F, StudentID = F, Status = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CoursePrerequisiteCurriculumCourseStudentCourseRequestID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CoursePrerequisiteCurriculumCourseStudentCourseRequest", objectId = CoursePrerequisiteCurriculumCourseStudentCourseRequestID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CoursePrerequisiteCurriculumCourses
	#'
	#' This function returns a dataframe or json object of CoursePrerequisiteCurriculumCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CoursePrerequisiteCurriculumCourse') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CoursePrerequisiteCurriculumCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCoursePrerequisiteCurriculumCourses <- function(searchConditionsList = NULL, CoursePrerequisiteCurriculumCourseID = F, CourseIDFor = F, EntityIDFor = F, SchoolYearIDFor = F, NumericYearCourse = F, NumericYearCurrentFor = F, CurriculumIDFor = F, PrerequisiteCurriculumID = F, PrerequisiteID = F, CurriculumIDRequired = F, CourseIDRequired = F, SchoolYearIDRequired = F, EntityIDRequired = F, NumericYearRequired = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CoursePrerequisiteCurriculumCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CoursePrerequisiteCurriculumCourse
	#'
	#' This function returns a dataframe or json object of a CoursePrerequisiteCurriculumCourse
	#' @param CoursePrerequisiteCurriculumCourseID The ID of the CoursePrerequisiteCurriculumCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CoursePrerequisiteCurriculumCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCoursePrerequisiteCurriculumCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCoursePrerequisiteCurriculumCourse <- function(CoursePrerequisiteCurriculumCourseID, CourseIDFor = F, EntityIDFor = F, SchoolYearIDFor = F, NumericYearCourse = F, NumericYearCurrentFor = F, CurriculumIDFor = F, PrerequisiteCurriculumID = F, PrerequisiteID = F, CurriculumIDRequired = F, CourseIDRequired = F, SchoolYearIDRequired = F, EntityIDRequired = F, NumericYearRequired = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CoursePrerequisiteCurriculumCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CoursePrerequisiteCurriculumCourse", objectId = CoursePrerequisiteCurriculumCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CoursePrerequisites
	#'
	#' This function returns a dataframe or json object of CoursePrerequisites
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CoursePrerequisite') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CoursePrerequisites
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCoursePrerequisites <- function(searchConditionsList = NULL, CoursePrerequisiteID = F, CourseID = F, EntityID = F, SchoolYearID = F, NumericYearCourse = F, NumericYearCurrent = F, CurriculumID = F, PrerequisiteID = F, EarnedCredits = F, SchoolYearLow = F, SchoolYearHigh = F, PrerequisiteCode = F, CurriculumCode = F, CurriculumDescription = F, HasPrequisiteCurriculums = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CoursePrerequisite", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CoursePrerequisite
	#'
	#' This function returns a dataframe or json object of a CoursePrerequisite
	#' @param CoursePrerequisiteID The ID of the CoursePrerequisite to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CoursePrerequisite') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCoursePrerequisite
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCoursePrerequisite <- function(CoursePrerequisiteID, CourseID = F, EntityID = F, SchoolYearID = F, NumericYearCourse = F, NumericYearCurrent = F, CurriculumID = F, PrerequisiteID = F, EarnedCredits = F, SchoolYearLow = F, SchoolYearHigh = F, PrerequisiteCode = F, CurriculumCode = F, CurriculumDescription = F, HasPrequisiteCurriculums = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CoursePrerequisiteID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CoursePrerequisite", objectId = CoursePrerequisiteID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PrerequisiteCurriculumCourses
	#'
	#' This function returns a dataframe or json object of PrerequisiteCurriculumCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('PrerequisiteCurriculumCourse') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of PrerequisiteCurriculumCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPrerequisiteCurriculumCourses <- function(searchConditionsList = NULL, PrerequisiteCurriculumCourseID = F, PrerequisiteCurriculumID = F, PrerequisiteID = F, CurriculumIDFor = F, CurriculumIDRequired = F, CourseID = F, SchoolYearID = F, EntityID = F, NumericYear = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "PrerequisiteCurriculumCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get PrerequisiteCurriculumCourse
	#'
	#' This function returns a dataframe or json object of a PrerequisiteCurriculumCourse
	#' @param PrerequisiteCurriculumCourseID The ID of the PrerequisiteCurriculumCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('PrerequisiteCurriculumCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getPrerequisiteCurriculumCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPrerequisiteCurriculumCourse <- function(PrerequisiteCurriculumCourseID, PrerequisiteCurriculumID = F, PrerequisiteID = F, CurriculumIDFor = F, CurriculumIDRequired = F, CourseID = F, SchoolYearID = F, EntityID = F, NumericYear = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PrerequisiteCurriculumCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "PrerequisiteCurriculumCourse", objectId = PrerequisiteCurriculumCourseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AvailabilityFilterArenaSchedulingSettings
	#'
	#' This function returns a dataframe or json object of AvailabilityFilterArenaSchedulingSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AvailabilityFilterArenaSchedulingSetting') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of AvailabilityFilterArenaSchedulingSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAvailabilityFilterArenaSchedulingSettings <- function(searchConditionsList = NULL, AvailabilityFilterArenaSchedulingSettingID = F, AvailableStartDate = F, AvailableEndDate = F, SchedulingEntryStartDateTime = F, SchedulingEntryEndDateTime = F, AvailabilityFilterCourseStudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HideStaff = F, HideRoom = F, ShowAllClasses = F, ShowMyRequests = F, ShowMyAlternateRequests = F, LockAfterSubmission = F, EnableAutoSchedule = F, AutoSchedulerCourseSelection = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AvailabilityFilterArenaSchedulingSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get AvailabilityFilterArenaSchedulingSetting
	#'
	#' This function returns a dataframe or json object of an AvailabilityFilterArenaSchedulingSetting
	#' @param AvailabilityFilterArenaSchedulingSettingID The ID of the AvailabilityFilterArenaSchedulingSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AvailabilityFilterArenaSchedulingSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getAvailabilityFilterArenaSchedulingSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAvailabilityFilterArenaSchedulingSetting <- function(AvailabilityFilterArenaSchedulingSettingID, AvailableStartDate = F, AvailableEndDate = F, SchedulingEntryStartDateTime = F, SchedulingEntryEndDateTime = F, AvailabilityFilterCourseStudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HideStaff = F, HideRoom = F, ShowAllClasses = F, ShowMyRequests = F, ShowMyAlternateRequests = F, LockAfterSubmission = F, EnableAutoSchedule = F, AutoSchedulerCourseSelection = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AvailabilityFilterArenaSchedulingSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "AvailabilityFilterArenaSchedulingSetting", objectId = AvailabilityFilterArenaSchedulingSettingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AvailabilityFilterCourseRequestSettings
	#'
	#' This function returns a dataframe or json object of AvailabilityFilterCourseRequestSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AvailabilityFilterCourseRequestSetting') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of AvailabilityFilterCourseRequestSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAvailabilityFilterCourseRequestSettings <- function(searchConditionsList = NULL, AvailabilityFilterCourseRequestSettingID = F, AvailableStartDate = F, AvailableEndDate = F, RequestEntryStartDateTime = F, RequestEntryEndDateTime = F, UseCreditsMaximum = F, UseCourseRequestCountMaximum = F, MaximumCredits = F, MaximumAlternateCredits = F, MaximumCourseRequests = F, MaximumAlternateCourseRequests = F, AvailabilityFilterCourseStudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AvailabilityFilterCourseRequestSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get AvailabilityFilterCourseRequestSetting
	#'
	#' This function returns a dataframe or json object of an AvailabilityFilterCourseRequestSetting
	#' @param AvailabilityFilterCourseRequestSettingID The ID of the AvailabilityFilterCourseRequestSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AvailabilityFilterCourseRequestSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getAvailabilityFilterCourseRequestSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAvailabilityFilterCourseRequestSetting <- function(AvailabilityFilterCourseRequestSettingID, AvailableStartDate = F, AvailableEndDate = F, RequestEntryStartDateTime = F, RequestEntryEndDateTime = F, UseCreditsMaximum = F, UseCourseRequestCountMaximum = F, MaximumCredits = F, MaximumAlternateCredits = F, MaximumCourseRequests = F, MaximumAlternateCourseRequests = F, AvailabilityFilterCourseStudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AvailabilityFilterCourseRequestSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "AvailabilityFilterCourseRequestSetting", objectId = AvailabilityFilterCourseRequestSettingID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MassUpdateSystemDateRunHistoryDetails
	#'
	#' This function returns a dataframe or json object of MassUpdateSystemDateRunHistoryDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MassUpdateSystemDateRunHistoryDetail') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of MassUpdateSystemDateRunHistoryDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMassUpdateSystemDateRunHistoryDetails <- function(searchConditionsList = NULL, MassUpdateSystemDateRunHistoryDetailID = F, MassUpdateSystemDateRunHistoryID = F, SortNumber = F, TableType = F, StartDateRecordsProcessedCount = F, StartDateSuccessCount = F, StartDateFailedCount = F, EndDateRecordsProcessedCount = F, EndDateSuccessCount = F, EndDateFailedCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MassUpdateSystemDateRunHistoryDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get MassUpdateSystemDateRunHistoryDetail
	#'
	#' This function returns a dataframe or json object of a MassUpdateSystemDateRunHistoryDetail
	#' @param MassUpdateSystemDateRunHistoryDetailID The ID of the MassUpdateSystemDateRunHistoryDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MassUpdateSystemDateRunHistoryDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getMassUpdateSystemDateRunHistoryDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMassUpdateSystemDateRunHistoryDetail <- function(MassUpdateSystemDateRunHistoryDetailID, MassUpdateSystemDateRunHistoryID = F, SortNumber = F, TableType = F, StartDateRecordsProcessedCount = F, StartDateSuccessCount = F, StartDateFailedCount = F, EndDateRecordsProcessedCount = F, EndDateSuccessCount = F, EndDateFailedCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MassUpdateSystemDateRunHistoryDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "MassUpdateSystemDateRunHistoryDetail", objectId = MassUpdateSystemDateRunHistoryDetailID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MassUpdateSystemDateRunHistoryErrors
	#'
	#' This function returns a dataframe or json object of MassUpdateSystemDateRunHistoryErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MassUpdateSystemDateRunHistoryError') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of MassUpdateSystemDateRunHistoryErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMassUpdateSystemDateRunHistoryErrors <- function(searchConditionsList = NULL, MassUpdateSystemDateRunHistoryErrorID = F, MassUpdateSystemDateRunHistoryID = F, ParentObjectID = F, ObjectID = F, Description = F, ErrorDetail = F, TableType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MassUpdateSystemDateRunHistoryError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get MassUpdateSystemDateRunHistoryError
	#'
	#' This function returns a dataframe or json object of a MassUpdateSystemDateRunHistoryError
	#' @param MassUpdateSystemDateRunHistoryErrorID The ID of the MassUpdateSystemDateRunHistoryError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MassUpdateSystemDateRunHistoryError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getMassUpdateSystemDateRunHistoryError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMassUpdateSystemDateRunHistoryError <- function(MassUpdateSystemDateRunHistoryErrorID, MassUpdateSystemDateRunHistoryID = F, ParentObjectID = F, ObjectID = F, Description = F, ErrorDetail = F, TableType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MassUpdateSystemDateRunHistoryErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "MassUpdateSystemDateRunHistoryError", objectId = MassUpdateSystemDateRunHistoryErrorID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MassUpdateSystemDateRunHistories
	#'
	#' This function returns a dataframe or json object of MassUpdateSystemDateRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MassUpdateSystemDateRunHistory') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of MassUpdateSystemDateRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMassUpdateSystemDateRunHistories <- function(searchConditionsList = NULL, MassUpdateSystemDateRunHistoryID = F, RunReason = F, RunDescription = F, DateTemplateXML = F, SchoolYearID = F, EntityID = F, StartTime = F, EndTime = F, Duration = F, Location = F, SourceID = F, HasErrors = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MassUpdateSystemDateRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get MassUpdateSystemDateRunHistory
	#'
	#' This function returns a dataframe or json object of a MassUpdateSystemDateRunHistory
	#' @param MassUpdateSystemDateRunHistoryID The ID of the MassUpdateSystemDateRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MassUpdateSystemDateRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getMassUpdateSystemDateRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMassUpdateSystemDateRunHistory <- function(MassUpdateSystemDateRunHistoryID, RunReason = F, RunDescription = F, DateTemplateXML = F, SchoolYearID = F, EntityID = F, StartTime = F, EndTime = F, Duration = F, Location = F, SourceID = F, HasErrors = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MassUpdateSystemDateRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "MassUpdateSystemDateRunHistory", objectId = MassUpdateSystemDateRunHistoryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMasterDateChangeDetails
	#'
	#' This function returns a dataframe or json object of TempMasterDateChangeDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempMasterDateChangeDetail') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of TempMasterDateChangeDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMasterDateChangeDetails <- function(searchConditionsList = NULL, TempMasterDateChangeDetailID = F, CurrentDate = F, NewDate = F, UsedBy = F, DateDescriptor = F, DisplayLowHighDates = F, LowDate = F, HighDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempMasterDateChangeDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempMasterDateChangeDetail
	#'
	#' This function returns a dataframe or json object of a TempMasterDateChangeDetail
	#' @param TempMasterDateChangeDetailID The ID of the TempMasterDateChangeDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempMasterDateChangeDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getTempMasterDateChangeDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMasterDateChangeDetail <- function(TempMasterDateChangeDetailID, CurrentDate = F, NewDate = F, UsedBy = F, DateDescriptor = F, DisplayLowHighDates = F, LowDate = F, HighDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMasterDateChangeDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "TempMasterDateChangeDetail", objectId = TempMasterDateChangeDetailID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseGradeReferenceSummaries
	#'
	#' This function returns a dataframe or json object of CourseGradeReferenceSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseGradeReferenceSummary') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseGradeReferenceSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseGradeReferenceSummaries <- function(searchConditionsList = NULL, CourseID = F, GradeLevelIDSummary = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseGradeReferenceSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseGradeReferenceSummary
	#'
	#' This function returns a dataframe or json object of a CourseGradeReferenceSummary
	#' @param CourseGradeReferenceSummaryID The ID of the CourseGradeReferenceSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseGradeReferenceSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseGradeReferenceSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseGradeReferenceSummary <- function(CourseGradeReferenceSummaryID, CourseID = F, GradeLevelIDSummary = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseGradeReferenceSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseGradeReferenceSummary", objectId = CourseGradeReferenceSummaryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CourseCorequisites
	#'
	#' This function returns a dataframe or json object of CourseCorequisites
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseCorequisite') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of CourseCorequisites
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCourseCorequisites <- function(searchConditionsList = NULL, CourseCorequisiteID = F, CourseIDParent = F, CourseCorequisiteGroupID = F, CourseIDMember = F, AutomaticRequestSetting = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseCorequisite", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get CourseCorequisite
	#'
	#' This function returns a dataframe or json object of a CourseCorequisite
	#' @param CourseCorequisiteID The ID of the CourseCorequisite to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('CourseCorequisite') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getCourseCorequisite
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCourseCorequisite <- function(CourseCorequisiteID, CourseIDParent = F, CourseCorequisiteGroupID = F, CourseIDMember = F, AutomaticRequestSetting = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CourseCorequisiteID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "CourseCorequisite", objectId = CourseCorequisiteID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AppleSchoolManagerConfigFileDestinations
	#'
	#' This function returns a dataframe or json object of AppleSchoolManagerConfigFileDestinations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AppleSchoolManagerConfigFileDestination') to get more field paths.
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
	#' @concept Scheduling
	#' @return A list of AppleSchoolManagerConfigFileDestinations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAppleSchoolManagerConfigFileDestinations <- function(searchConditionsList = NULL, AppleSchoolManagerConfigFileDestinationID = F, AppleSchoolManagerConfigID = F, FileDestinationID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AppleSchoolManagerConfigFileDestination", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get AppleSchoolManagerConfigFileDestination
	#'
	#' This function returns a dataframe or json object of an AppleSchoolManagerConfigFileDestination
	#' @param AppleSchoolManagerConfigFileDestinationID The ID of the AppleSchoolManagerConfigFileDestination to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('AppleSchoolManagerConfigFileDestination') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Scheduling
	#' @return A dataframe or of getAppleSchoolManagerConfigFileDestination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAppleSchoolManagerConfigFileDestination <- function(AppleSchoolManagerConfigFileDestinationID, AppleSchoolManagerConfigID = F, FileDestinationID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AppleSchoolManagerConfigFileDestinationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Scheduling", objectName = "AppleSchoolManagerConfigFileDestination", objectId = AppleSchoolManagerConfigFileDestinationID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}
