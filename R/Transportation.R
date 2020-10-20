
	#' List Buses
	#'
	#' This function returns a dataframe or json object of Buses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Buses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Buses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Bus') to get more field paths.
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
	#' @concept Transportation
	#' @return A list of Buses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBuses <- function(searchConditionsList = NULL, BusID = F, DistrictID = F, Code = F, Description = F, Capacity = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasHistoricBusRoutes = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Transportation", objectName = "Bus", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Bus
	#'
	#' This function returns a dataframe or json object of a Bus
	#' @param BusID The ID of the Bus to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Bus. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Bus.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Bus') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A dataframe or of Bus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBus <- function(BusID, DistrictID = F, Code = F, Description = F, Capacity = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasHistoricBusRoutes = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BusID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Transportation", objectName = "Bus", objectId = BusID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Bus
	#'
	#' This function deletes a Bus
	#' @param BusID The ID of the Bus to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The BusID of the deleted Bus.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBus <- function(BusID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Transportation", objectName = "Bus", objectId = BusID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Bus
	#'
	#' This function creates a Bus
	#' @param fieldNames The field values to give the created Bus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A newly created Bus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBus <- function(DistrictID = NULL, Code = NULL, Description = NULL, Capacity = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Transportation", objectName = "Bus", body = list(DataObject = body), searchFields = append("BusID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Bus
	#'
	#' This function modifies a Bus
	#' @param fieldNames The field values to give the modified Bus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The modified Bus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBus <- function(BusID, DistrictID = NULL, Code = NULL, Description = NULL, Capacity = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Transportation", objectName = "Bus", objectId = BusID, body = list(DataObject = body), searchFields = append("BusID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TransportationCategories
	#'
	#' This function returns a dataframe or json object of TransportationCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TransportationCategories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TransportationCategories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TransportationCategory') to get more field paths.
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
	#' @concept Transportation
	#' @return A list of TransportationCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTransportationCategories <- function(searchConditionsList = NULL, TransportationCategoryID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SchoolYearID = F, TransportationCategoryIDClonedFrom = F, TransportationCategoryMNID = F, StateTransportationCategoryCodeMNID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Transportation", objectName = "TransportationCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TransportationCategory
	#'
	#' This function returns a dataframe or json object of a TransportationCategory
	#' @param TransportationCategoryID The ID of the TransportationCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TransportationCategory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TransportationCategory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TransportationCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A dataframe or of TransportationCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTransportationCategory <- function(TransportationCategoryID, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SchoolYearID = F, TransportationCategoryIDClonedFrom = F, TransportationCategoryMNID = F, StateTransportationCategoryCodeMNID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TransportationCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Transportation", objectName = "TransportationCategory", objectId = TransportationCategoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TransportationCategory
	#'
	#' This function deletes a TransportationCategory
	#' @param TransportationCategoryID The ID of the TransportationCategory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The TransportationCategoryID of the deleted TransportationCategory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTransportationCategory <- function(TransportationCategoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Transportation", objectName = "TransportationCategory", objectId = TransportationCategoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TransportationCategory
	#'
	#' This function creates a TransportationCategory
	#' @param fieldNames The field values to give the created TransportationCategory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A newly created TransportationCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTransportationCategory <- function(DistrictID = NULL, Code = NULL, Description = NULL, SchoolYearID = NULL, StateTransportationCategoryCodeMNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Transportation", objectName = "TransportationCategory", body = list(DataObject = body), searchFields = append("TransportationCategoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TransportationCategory
	#'
	#' This function modifies a TransportationCategory
	#' @param fieldNames The field values to give the modified TransportationCategory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The modified TransportationCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTransportationCategory <- function(TransportationCategoryID, DistrictID = NULL, Code = NULL, Description = NULL, SchoolYearID = NULL, StateTransportationCategoryCodeMNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Transportation", objectName = "TransportationCategory", objectId = TransportationCategoryID, body = list(DataObject = body), searchFields = append("TransportationCategoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BusRoutes
	#'
	#' This function returns a dataframe or json object of BusRoutes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BusRoutes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BusRoutes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BusRoute') to get more field paths.
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
	#' @concept Transportation
	#' @return A list of BusRoutes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBusRoutes <- function(searchConditionsList = NULL, Mileage = F, BusRouteID = F, NameIDDriver = F, DepartureTime = F, ArrivalTime = F, BusID = F, RouteType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasBusStops = F, SchoolYearID = F, Description = F, BusRouteIDClonedFrom = F, StudentCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Transportation", objectName = "BusRoute", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BusRoute
	#'
	#' This function returns a dataframe or json object of a BusRoute
	#' @param BusRouteID The ID of the BusRoute to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BusRoute. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BusRoute.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BusRoute') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A dataframe or of BusRoute
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBusRoute <- function(BusRouteID, Mileage = F, NameIDDriver = F, DepartureTime = F, ArrivalTime = F, BusID = F, RouteType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasBusStops = F, SchoolYearID = F, Description = F, BusRouteIDClonedFrom = F, StudentCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BusRouteID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Transportation", objectName = "BusRoute", objectId = BusRouteID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BusRoute
	#'
	#' This function deletes a BusRoute
	#' @param BusRouteID The ID of the BusRoute to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The BusRouteID of the deleted BusRoute.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBusRoute <- function(BusRouteID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Transportation", objectName = "BusRoute", objectId = BusRouteID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BusRoute
	#'
	#' This function creates a BusRoute
	#' @param fieldNames The field values to give the created BusRoute. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A newly created BusRoute
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBusRoute <- function(Mileage = NULL, NameIDDriver = NULL, DepartureTime = NULL, ArrivalTime = NULL, BusID = NULL, RouteType = NULL, SchoolYearID = NULL, Description = NULL, BusRouteIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Transportation", objectName = "BusRoute", body = list(DataObject = body), searchFields = append("BusRouteID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BusRoute
	#'
	#' This function modifies a BusRoute
	#' @param fieldNames The field values to give the modified BusRoute. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The modified BusRoute
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBusRoute <- function(BusRouteID, Mileage = NULL, NameIDDriver = NULL, DepartureTime = NULL, ArrivalTime = NULL, BusID = NULL, RouteType = NULL, SchoolYearID = NULL, Description = NULL, BusRouteIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Transportation", objectName = "BusRoute", objectId = BusRouteID, body = list(DataObject = body), searchFields = append("BusRouteID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentTransportations
	#'
	#' This function returns a dataframe or json object of StudentTransportations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentTransportations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentTransportations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentTransportation') to get more field paths.
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
	#' @concept Transportation
	#' @return A list of StudentTransportations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentTransportations <- function(searchConditionsList = NULL, StudentTransportationID = F, DistrictID = F, SchoolYearID = F, StartDate = F, EndDate = F, Transported = F, Miles = F, TransportationCategoryID = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentTransportationMNID = F, StateDistrictMNID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Transportation", objectName = "StudentTransportation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentTransportation
	#'
	#' This function returns a dataframe or json object of a StudentTransportation
	#' @param StudentTransportationID The ID of the StudentTransportation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentTransportation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentTransportation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentTransportation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A dataframe or of StudentTransportation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentTransportation <- function(StudentTransportationID, DistrictID = F, SchoolYearID = F, StartDate = F, EndDate = F, Transported = F, Miles = F, TransportationCategoryID = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentTransportationMNID = F, StateDistrictMNID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentTransportationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Transportation", objectName = "StudentTransportation", objectId = StudentTransportationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentTransportation
	#'
	#' This function deletes a StudentTransportation
	#' @param StudentTransportationID The ID of the StudentTransportation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The StudentTransportationID of the deleted StudentTransportation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentTransportation <- function(StudentTransportationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Transportation", objectName = "StudentTransportation", objectId = StudentTransportationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentTransportation
	#'
	#' This function creates a StudentTransportation
	#' @param fieldNames The field values to give the created StudentTransportation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A newly created StudentTransportation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentTransportation <- function(DistrictID = NULL, SchoolYearID = NULL, StartDate = NULL, EndDate = NULL, Transported = NULL, Miles = NULL, TransportationCategoryID = NULL, StudentID = NULL, StateDistrictMNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Transportation", objectName = "StudentTransportation", body = list(DataObject = body), searchFields = append("StudentTransportationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentTransportation
	#'
	#' This function modifies a StudentTransportation
	#' @param fieldNames The field values to give the modified StudentTransportation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The modified StudentTransportation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentTransportation <- function(StudentTransportationID, DistrictID = NULL, SchoolYearID = NULL, StartDate = NULL, EndDate = NULL, Transported = NULL, Miles = NULL, TransportationCategoryID = NULL, StudentID = NULL, StateDistrictMNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Transportation", objectName = "StudentTransportation", objectId = StudentTransportationID, body = list(DataObject = body), searchFields = append("StudentTransportationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BusStops
	#'
	#' This function returns a dataframe or json object of BusStops
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BusStops. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BusStops.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BusStop') to get more field paths.
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
	#' @concept Transportation
	#' @return A list of BusStops
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBusStops <- function(searchConditionsList = NULL, BusStopID = F, ArrivalTime = F, DepartureTime = F, AddressID = F, Description = F, StopNumber = F, BusRouteID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BusStopIDClonedFrom = F, ArrivalTimeFormatted = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Transportation", objectName = "BusStop", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BusStop
	#'
	#' This function returns a dataframe or json object of a BusStop
	#' @param BusStopID The ID of the BusStop to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BusStop. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BusStop.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BusStop') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A dataframe or of BusStop
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBusStop <- function(BusStopID, ArrivalTime = F, DepartureTime = F, AddressID = F, Description = F, StopNumber = F, BusRouteID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BusStopIDClonedFrom = F, ArrivalTimeFormatted = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BusStopID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Transportation", objectName = "BusStop", objectId = BusStopID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BusStop
	#'
	#' This function deletes a BusStop
	#' @param BusStopID The ID of the BusStop to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The BusStopID of the deleted BusStop.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBusStop <- function(BusStopID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Transportation", objectName = "BusStop", objectId = BusStopID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BusStop
	#'
	#' This function creates a BusStop
	#' @param fieldNames The field values to give the created BusStop. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A newly created BusStop
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBusStop <- function(ArrivalTime = NULL, DepartureTime = NULL, AddressID = NULL, Description = NULL, StopNumber = NULL, BusRouteID = NULL, BusStopIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Transportation", objectName = "BusStop", body = list(DataObject = body), searchFields = append("BusStopID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BusStop
	#'
	#' This function modifies a BusStop
	#' @param fieldNames The field values to give the modified BusStop. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The modified BusStop
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBusStop <- function(BusStopID, ArrivalTime = NULL, DepartureTime = NULL, AddressID = NULL, Description = NULL, StopNumber = NULL, BusRouteID = NULL, BusStopIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Transportation", objectName = "BusStop", objectId = BusStopID, body = list(DataObject = body), searchFields = append("BusStopID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentBusStops
	#'
	#' This function returns a dataframe or json object of StudentBusStops
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentBusStops. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentBusStops.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentBusStop') to get more field paths.
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
	#' @concept Transportation
	#' @return A list of StudentBusStops
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentBusStops <- function(searchConditionsList = NULL, StudentBusStopID = F, BusStopID = F, StudentID = F, StopType = F, IsSunday = F, IsMonday = F, IsTuesday = F, IsWednesday = F, IsThursday = F, IsFriday = F, IsSaturday = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasCurrentDayOfWeek = F, IsCurrentOrFutureTransported = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Transportation", objectName = "StudentBusStop", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentBusStop
	#'
	#' This function returns a dataframe or json object of a StudentBusStop
	#' @param StudentBusStopID The ID of the StudentBusStop to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentBusStop. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentBusStop.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentBusStop') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A dataframe or of StudentBusStop
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentBusStop <- function(StudentBusStopID, BusStopID = F, StudentID = F, StopType = F, IsSunday = F, IsMonday = F, IsTuesday = F, IsWednesday = F, IsThursday = F, IsFriday = F, IsSaturday = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasCurrentDayOfWeek = F, IsCurrentOrFutureTransported = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentBusStopID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Transportation", objectName = "StudentBusStop", objectId = StudentBusStopID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentBusStop
	#'
	#' This function deletes a StudentBusStop
	#' @param StudentBusStopID The ID of the StudentBusStop to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The StudentBusStopID of the deleted StudentBusStop.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentBusStop <- function(StudentBusStopID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Transportation", objectName = "StudentBusStop", objectId = StudentBusStopID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentBusStop
	#'
	#' This function creates a StudentBusStop
	#' @param fieldNames The field values to give the created StudentBusStop. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A newly created StudentBusStop
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentBusStop <- function(BusStopID = NULL, StudentID = NULL, StopType = NULL, IsSunday = NULL, IsMonday = NULL, IsTuesday = NULL, IsWednesday = NULL, IsThursday = NULL, IsFriday = NULL, IsSaturday = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Transportation", objectName = "StudentBusStop", body = list(DataObject = body), searchFields = append("StudentBusStopID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentBusStop
	#'
	#' This function modifies a StudentBusStop
	#' @param fieldNames The field values to give the modified StudentBusStop. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The modified StudentBusStop
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentBusStop <- function(StudentBusStopID, BusStopID = NULL, StudentID = NULL, StopType = NULL, IsSunday = NULL, IsMonday = NULL, IsTuesday = NULL, IsWednesday = NULL, IsThursday = NULL, IsFriday = NULL, IsSaturday = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Transportation", objectName = "StudentBusStop", objectId = StudentBusStopID, body = list(DataObject = body), searchFields = append("StudentBusStopID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentBusStops
	#'
	#' This function returns a dataframe or json object of TempStudentBusStops
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentBusStops. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentBusStops.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentBusStop') to get more field paths.
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
	#' @concept Transportation
	#' @return A list of TempStudentBusStops
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentBusStops <- function(searchConditionsList = NULL, TempStudentBusStopID = F, StudentBusStopID = F, StudentNameLFM = F, StudentNumber = F, BusStopDescription = F, BusStopArrivalTime = F, BusStopFullAddress = F, HasExceptions = F, Exceptions = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BusStopID = F, StudentID = F, IsAccepted = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Transportation", objectName = "TempStudentBusStop", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentBusStop
	#'
	#' This function returns a dataframe or json object of a TempStudentBusStop
	#' @param TempStudentBusStopID The ID of the TempStudentBusStop to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentBusStop. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentBusStop.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentBusStop') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A dataframe or of TempStudentBusStop
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentBusStop <- function(TempStudentBusStopID, StudentBusStopID = F, StudentNameLFM = F, StudentNumber = F, BusStopDescription = F, BusStopArrivalTime = F, BusStopFullAddress = F, HasExceptions = F, Exceptions = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BusStopID = F, StudentID = F, IsAccepted = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentBusStopID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Transportation", objectName = "TempStudentBusStop", objectId = TempStudentBusStopID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentBusStop
	#'
	#' This function deletes a TempStudentBusStop
	#' @param TempStudentBusStopID The ID of the TempStudentBusStop to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The TempStudentBusStopID of the deleted TempStudentBusStop.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentBusStop <- function(TempStudentBusStopID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Transportation", objectName = "TempStudentBusStop", objectId = TempStudentBusStopID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentBusStop
	#'
	#' This function creates a TempStudentBusStop
	#' @param fieldNames The field values to give the created TempStudentBusStop. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A newly created TempStudentBusStop
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentBusStop <- function(StudentNameLFM = NULL, StudentNumber = NULL, BusStopDescription = NULL, BusStopArrivalTime = NULL, BusStopFullAddress = NULL, Exceptions = NULL, IsAccepted = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Transportation", objectName = "TempStudentBusStop", body = list(DataObject = body), searchFields = append("TempStudentBusStopID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentBusStop
	#'
	#' This function modifies a TempStudentBusStop
	#' @param fieldNames The field values to give the modified TempStudentBusStop. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The modified TempStudentBusStop
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentBusStop <- function(TempStudentBusStopID, StudentNameLFM = NULL, StudentNumber = NULL, BusStopDescription = NULL, BusStopArrivalTime = NULL, BusStopFullAddress = NULL, Exceptions = NULL, IsAccepted = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Transportation", objectName = "TempStudentBusStop", objectId = TempStudentBusStopID, body = list(DataObject = body), searchFields = append("TempStudentBusStopID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TransportationConfigDistricts
	#'
	#' This function returns a dataframe or json object of TransportationConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TransportationConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TransportationConfigDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TransportationConfigDistrict') to get more field paths.
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
	#' @concept Transportation
	#' @return A list of TransportationConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTransportationConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TransportationImportFileName = F, FileDestinationIDTransportationImport = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Transportation", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TransportationConfigDistrict
	#'
	#' This function returns a dataframe or json object of a TransportationConfigDistrict
	#' @param TransportationConfigDistrictID The ID of the TransportationConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TransportationConfigDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TransportationConfigDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TransportationConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A dataframe or of TransportationConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTransportationConfigDistrict <- function(TransportationConfigDistrictID, ConfigDistrictID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TransportationImportFileName = F, FileDestinationIDTransportationImport = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TransportationConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Transportation", objectName = "ConfigDistrict", objectId = TransportationConfigDistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TransportationConfigDistrict
	#'
	#' This function deletes a TransportationConfigDistrict
	#' @param TransportationConfigDistrictID The ID of the TransportationConfigDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The TransportationConfigDistrictID of the deleted TransportationConfigDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTransportationConfigDistrict <- function(TransportationConfigDistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Transportation", objectName = "ConfigDistrict", objectId = TransportationConfigDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TransportationConfigDistrict
	#'
	#' This function creates a TransportationConfigDistrict
	#' @param fieldNames The field values to give the created TransportationConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A newly created TransportationConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTransportationConfigDistrict <- function(DistrictID = NULL, TransportationImportFileName = NULL, FileDestinationIDTransportationImport = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Transportation", objectName = "ConfigDistrict", body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TransportationConfigDistrict
	#'
	#' This function modifies a TransportationConfigDistrict
	#' @param fieldNames The field values to give the modified TransportationConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The modified TransportationConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTransportationConfigDistrict <- function(ConfigDistrictID, DistrictID = NULL, TransportationImportFileName = NULL, FileDestinationIDTransportationImport = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Transportation", objectName = "ConfigDistrict", objectId = ConfigDistrictID, body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentTransportations
	#'
	#' This function returns a dataframe or json object of TempStudentTransportations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentTransportations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentTransportations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentTransportation') to get more field paths.
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
	#' @concept Transportation
	#' @return A list of TempStudentTransportations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentTransportations <- function(searchConditionsList = NULL, TempStudentTransportationID = F, StudentTransportationID = F, StudentNameLFM = F, StudentNumber = F, TransportationCategoryDescription = F, StartDate = F, HasExceptions = F, Exceptions = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Transportation", objectName = "TempStudentTransportation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentTransportation
	#'
	#' This function returns a dataframe or json object of a TempStudentTransportation
	#' @param TempStudentTransportationID The ID of the TempStudentTransportation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentTransportation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentTransportation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentTransportation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A dataframe or of TempStudentTransportation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentTransportation <- function(TempStudentTransportationID, StudentTransportationID = F, StudentNameLFM = F, StudentNumber = F, TransportationCategoryDescription = F, StartDate = F, HasExceptions = F, Exceptions = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentTransportationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Transportation", objectName = "TempStudentTransportation", objectId = TempStudentTransportationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentTransportation
	#'
	#' This function deletes a TempStudentTransportation
	#' @param TempStudentTransportationID The ID of the TempStudentTransportation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The TempStudentTransportationID of the deleted TempStudentTransportation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentTransportation <- function(TempStudentTransportationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Transportation", objectName = "TempStudentTransportation", objectId = TempStudentTransportationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentTransportation
	#'
	#' This function creates a TempStudentTransportation
	#' @param fieldNames The field values to give the created TempStudentTransportation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A newly created TempStudentTransportation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentTransportation <- function(StudentNameLFM = NULL, StudentNumber = NULL, TransportationCategoryDescription = NULL, StartDate = NULL, Exceptions = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Transportation", objectName = "TempStudentTransportation", body = list(DataObject = body), searchFields = append("TempStudentTransportationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentTransportation
	#'
	#' This function modifies a TempStudentTransportation
	#' @param fieldNames The field values to give the modified TempStudentTransportation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The modified TempStudentTransportation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentTransportation <- function(TempStudentTransportationID, StudentNameLFM = NULL, StudentNumber = NULL, TransportationCategoryDescription = NULL, StartDate = NULL, Exceptions = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Transportation", objectName = "TempStudentTransportation", objectId = TempStudentTransportationID, body = list(DataObject = body), searchFields = append("TempStudentTransportationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempTransportations
	#'
	#' This function returns a dataframe or json object of TempTransportations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTransportations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTransportations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTransportation') to get more field paths.
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
	#' @concept Transportation
	#' @return A list of TempTransportations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempTransportations <- function(searchConditionsList = NULL, TempTransportationID = F, HasExceptions = F, Exceptions = F, StudentNumber = F, FirstName = F, LastName = F, StudentAddress = F, School = F, GradeLevel = F, TransportationCategory = F, Bus = F, BusRoute = F, Driver = F, BusStop = F, BusStopAddress = F, Days = F, StopType = F, ArrivalTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Transportation", objectName = "TempTransportation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempTransportation
	#'
	#' This function returns a dataframe or json object of a TempTransportation
	#' @param TempTransportationID The ID of the TempTransportation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTransportation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTransportation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTransportation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A dataframe or of TempTransportation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempTransportation <- function(TempTransportationID, HasExceptions = F, Exceptions = F, StudentNumber = F, FirstName = F, LastName = F, StudentAddress = F, School = F, GradeLevel = F, TransportationCategory = F, Bus = F, BusRoute = F, Driver = F, BusStop = F, BusStopAddress = F, Days = F, StopType = F, ArrivalTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempTransportationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Transportation", objectName = "TempTransportation", objectId = TempTransportationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempTransportation
	#'
	#' This function deletes a TempTransportation
	#' @param TempTransportationID The ID of the TempTransportation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The TempTransportationID of the deleted TempTransportation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempTransportation <- function(TempTransportationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Transportation", objectName = "TempTransportation", objectId = TempTransportationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempTransportation
	#'
	#' This function creates a TempTransportation
	#' @param fieldNames The field values to give the created TempTransportation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return A newly created TempTransportation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempTransportation <- function(HasExceptions = NULL, Exceptions = NULL, StudentNumber = NULL, FirstName = NULL, LastName = NULL, StudentAddress = NULL, School = NULL, GradeLevel = NULL, TransportationCategory = NULL, Bus = NULL, BusRoute = NULL, Driver = NULL, BusStop = NULL, BusStopAddress = NULL, Days = NULL, StopType = NULL, ArrivalTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Transportation", objectName = "TempTransportation", body = list(DataObject = body), searchFields = append("TempTransportationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempTransportation
	#'
	#' This function modifies a TempTransportation
	#' @param fieldNames The field values to give the modified TempTransportation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Transportation
	#' @return The modified TempTransportation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempTransportation <- function(TempTransportationID, HasExceptions = NULL, Exceptions = NULL, StudentNumber = NULL, FirstName = NULL, LastName = NULL, StudentAddress = NULL, School = NULL, GradeLevel = NULL, TransportationCategory = NULL, Bus = NULL, BusRoute = NULL, Driver = NULL, BusStop = NULL, BusStopAddress = NULL, Days = NULL, StopType = NULL, ArrivalTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Transportation", objectName = "TempTransportation", objectId = TempTransportationID, body = list(DataObject = body), searchFields = append("TempTransportationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
