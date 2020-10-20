
	#' List Buildings
	#'
	#' This function returns a dataframe or json object of Buildings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Buildings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Buildings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Building') to get more field paths.
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
	#' @concept District
	#' @return A list of Buildings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBuildings <- function(searchConditionsList = NULL, BuildingMNID = F, UnemploymentInsuranceUnitLocation = F, STARSchoolNumber = F, BuildingID = F, DistrictID = F, AddressID = F, Code = F, Description = F, ParcelNumber = F, MinimumStudentCount = F, MaximumStudentCount = F, OptimumStudentCount = F, CodeDescription = F, AccountDistributionString = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "Building", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Building
	#'
	#' This function returns a dataframe or json object of a Building
	#' @param BuildingID The ID of the Building to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Building. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Building.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Building') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of Building
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBuilding <- function(BuildingID, BuildingMNID = F, UnemploymentInsuranceUnitLocation = F, STARSchoolNumber = F, DistrictID = F, AddressID = F, Code = F, Description = F, ParcelNumber = F, MinimumStudentCount = F, MaximumStudentCount = F, OptimumStudentCount = F, CodeDescription = F, AccountDistributionString = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BuildingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "Building", objectId = BuildingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Building
	#'
	#' This function deletes a Building
	#' @param BuildingID The ID of the Building to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The BuildingID of the deleted Building.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBuilding <- function(BuildingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "Building", objectId = BuildingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Building
	#'
	#' This function creates a Building
	#' @param fieldNames The field values to give the created Building. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created Building
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBuilding <- function(UnemploymentInsuranceUnitLocation = NULL, STARSchoolNumber = NULL, DistrictID = NULL, AddressID = NULL, Code = NULL, Description = NULL, ParcelNumber = NULL, MinimumStudentCount = NULL, MaximumStudentCount = NULL, OptimumStudentCount = NULL, FederalNCESSchoolID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "Building", body = list(DataObject = body), searchFields = append("BuildingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Building
	#'
	#' This function modifies a Building
	#' @param fieldNames The field values to give the modified Building. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified Building
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBuilding <- function(BuildingID, UnemploymentInsuranceUnitLocation = NULL, STARSchoolNumber = NULL, DistrictID = NULL, AddressID = NULL, Code = NULL, Description = NULL, ParcelNumber = NULL, MinimumStudentCount = NULL, MaximumStudentCount = NULL, OptimumStudentCount = NULL, FederalNCESSchoolID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "Building", objectId = BuildingID, body = list(DataObject = body), searchFields = append("BuildingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Districts
	#'
	#' This function returns a dataframe or json object of Districts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Districts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Districts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('District') to get more field paths.
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
	#' @concept District
	#' @return A list of Districts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDistricts <- function(searchConditionsList = NULL, DistrictMNID = F, StateDistrictMNID = F, DistrictID = F, Name = F, DistrictGroupID = F, StaffIDSuperintendent = F, BuildingID = F, PhoneNumberIsInternational = F, PhoneNumber = F, FaxNumberIsInternational = F, FaxNumber = F, CodeName = F, StateDistrictCode = F, RCDTCodeBySchoolYear = F, DistrictCodeBySchoolYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DistrictNumber = F, StateDistrictTypeCodeMNID = F, FormattedPhoneNumber = F, NCESIDCode = F, IsCurrentlySelected = F, HasOrganizationChartTimeOffApprovalTask = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "District", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a District
	#'
	#' This function returns a dataframe or json object of a District
	#' @param DistrictID The ID of the District to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given District. Defaults to FALSE for all return fields which, for convenience, returns all fields for the District.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('District') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of District
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDistrict <- function(DistrictID, DistrictMNID = F, StateDistrictMNID = F, Name = F, DistrictGroupID = F, StaffIDSuperintendent = F, BuildingID = F, PhoneNumberIsInternational = F, PhoneNumber = F, FaxNumberIsInternational = F, FaxNumber = F, CodeName = F, StateDistrictCode = F, RCDTCodeBySchoolYear = F, DistrictCodeBySchoolYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DistrictNumber = F, StateDistrictTypeCodeMNID = F, FormattedPhoneNumber = F, NCESIDCode = F, IsCurrentlySelected = F, HasOrganizationChartTimeOffApprovalTask = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "District", objectId = DistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a District
	#'
	#' This function deletes a District
	#' @param DistrictID The ID of the District to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The DistrictID of the deleted District.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDistrict <- function(DistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "District", objectId = DistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a District
	#'
	#' This function creates a District
	#' @param fieldNames The field values to give the created District. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created District
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDistrict <- function(StateDistrictMNID = NULL, Name = NULL, DistrictGroupID = NULL, StaffIDSuperintendent = NULL, BuildingID = NULL, PhoneNumberIsInternational = NULL, PhoneNumber = NULL, FaxNumberIsInternational = NULL, FaxNumber = NULL, DistrictNumber = NULL, StateDistrictTypeCodeMNID = NULL, NCESIDCode = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "District", body = list(DataObject = body), searchFields = append("DistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a District
	#'
	#' This function modifies a District
	#' @param fieldNames The field values to give the modified District. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified District
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDistrict <- function(DistrictID, StateDistrictMNID = NULL, Name = NULL, DistrictGroupID = NULL, StaffIDSuperintendent = NULL, BuildingID = NULL, PhoneNumberIsInternational = NULL, PhoneNumber = NULL, FaxNumberIsInternational = NULL, FaxNumber = NULL, DistrictNumber = NULL, StateDistrictTypeCodeMNID = NULL, NCESIDCode = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "District", objectId = DistrictID, body = list(DataObject = body), searchFields = append("DistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DistrictSchoolYears
	#'
	#' This function returns a dataframe or json object of DistrictSchoolYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DistrictSchoolYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DistrictSchoolYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DistrictSchoolYear') to get more field paths.
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
	#' @concept District
	#' @return A list of DistrictSchoolYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDistrictSchoolYears <- function(searchConditionsList = NULL, DistrictSchoolYearID = F, DistrictID = F, SchoolYearID = F, DistrictSchoolYearIDClonedFrom = F, NameIDSex = F, NameIDRace = F, NameIDDisability = F, HasDesegregationPlan = F, HasHarassmentPolicy = F, HarassmentPolicyWebLink = F, HasEarlyChildhood = F, HasEarlyChildhoodNonIDEA = F, HasPreschool = F, HasPreschoolFullDayFree = F, HasPreschoolFullDayCost = F, HasPreschoolPartDayFree = F, HasPreschoolPartDayCost = F, HasPreschoolAllChildren = F, HasPreschoolIDEA = F, HasPreschoolTitleI = F, HasPreschoolLowIncome = F, HasKindergarten = F, HasKindergartenFullDayFree = F, HasKindergartenFullDayCost = F, HasKindergartenPartDayFree = F, HasKindergartenPartDayCost = F, HasDistanceEducation = F, HasGEDPreparationProgram = F, IsCRDCCollectedForSchoolYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EdFiDistrictID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "DistrictSchoolYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DistrictSchoolYear
	#'
	#' This function returns a dataframe or json object of a DistrictSchoolYear
	#' @param DistrictSchoolYearID The ID of the DistrictSchoolYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DistrictSchoolYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DistrictSchoolYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DistrictSchoolYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of DistrictSchoolYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDistrictSchoolYear <- function(DistrictSchoolYearID, DistrictID = F, SchoolYearID = F, DistrictSchoolYearIDClonedFrom = F, NameIDSex = F, NameIDRace = F, NameIDDisability = F, HasDesegregationPlan = F, HasHarassmentPolicy = F, HarassmentPolicyWebLink = F, HasEarlyChildhood = F, HasEarlyChildhoodNonIDEA = F, HasPreschool = F, HasPreschoolFullDayFree = F, HasPreschoolFullDayCost = F, HasPreschoolPartDayFree = F, HasPreschoolPartDayCost = F, HasPreschoolAllChildren = F, HasPreschoolIDEA = F, HasPreschoolTitleI = F, HasPreschoolLowIncome = F, HasKindergarten = F, HasKindergartenFullDayFree = F, HasKindergartenFullDayCost = F, HasKindergartenPartDayFree = F, HasKindergartenPartDayCost = F, HasDistanceEducation = F, HasGEDPreparationProgram = F, IsCRDCCollectedForSchoolYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EdFiDistrictID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DistrictSchoolYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "DistrictSchoolYear", objectId = DistrictSchoolYearID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DistrictSchoolYear
	#'
	#' This function deletes a DistrictSchoolYear
	#' @param DistrictSchoolYearID The ID of the DistrictSchoolYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The DistrictSchoolYearID of the deleted DistrictSchoolYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDistrictSchoolYear <- function(DistrictSchoolYearID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "DistrictSchoolYear", objectId = DistrictSchoolYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DistrictSchoolYear
	#'
	#' This function creates a DistrictSchoolYear
	#' @param fieldNames The field values to give the created DistrictSchoolYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created DistrictSchoolYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDistrictSchoolYear <- function(DistrictID = NULL, SchoolYearID = NULL, DistrictSchoolYearIDClonedFrom = NULL, NameIDSex = NULL, NameIDRace = NULL, NameIDDisability = NULL, HasDesegregationPlan = NULL, HasHarassmentPolicy = NULL, HarassmentPolicyWebLink = NULL, HasEarlyChildhood = NULL, HasEarlyChildhoodNonIDEA = NULL, HasPreschool = NULL, HasPreschoolFullDayFree = NULL, HasPreschoolFullDayCost = NULL, HasPreschoolPartDayFree = NULL, HasPreschoolPartDayCost = NULL, HasPreschoolAllChildren = NULL, HasPreschoolIDEA = NULL, HasPreschoolTitleI = NULL, HasPreschoolLowIncome = NULL, HasKindergarten = NULL, HasKindergartenFullDayFree = NULL, HasKindergartenFullDayCost = NULL, HasKindergartenPartDayFree = NULL, HasKindergartenPartDayCost = NULL, HasDistanceEducation = NULL, HasGEDPreparationProgram = NULL, EdFiDistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "DistrictSchoolYear", body = list(DataObject = body), searchFields = append("DistrictSchoolYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DistrictSchoolYear
	#'
	#' This function modifies a DistrictSchoolYear
	#' @param fieldNames The field values to give the modified DistrictSchoolYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified DistrictSchoolYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDistrictSchoolYear <- function(DistrictSchoolYearID, DistrictID = NULL, SchoolYearID = NULL, DistrictSchoolYearIDClonedFrom = NULL, NameIDSex = NULL, NameIDRace = NULL, NameIDDisability = NULL, HasDesegregationPlan = NULL, HasHarassmentPolicy = NULL, HarassmentPolicyWebLink = NULL, HasEarlyChildhood = NULL, HasEarlyChildhoodNonIDEA = NULL, HasPreschool = NULL, HasPreschoolFullDayFree = NULL, HasPreschoolFullDayCost = NULL, HasPreschoolPartDayFree = NULL, HasPreschoolPartDayCost = NULL, HasPreschoolAllChildren = NULL, HasPreschoolIDEA = NULL, HasPreschoolTitleI = NULL, HasPreschoolLowIncome = NULL, HasKindergarten = NULL, HasKindergartenFullDayFree = NULL, HasKindergartenFullDayCost = NULL, HasKindergartenPartDayFree = NULL, HasKindergartenPartDayCost = NULL, HasDistanceEducation = NULL, HasGEDPreparationProgram = NULL, EdFiDistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "DistrictSchoolYear", objectId = DistrictSchoolYearID, body = list(DataObject = body), searchFields = append("DistrictSchoolYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateDistrictMNS
	#'
	#' This function returns a dataframe or json object of StateDistrictMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateDistrictMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateDistrictMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateDistrictMN') to get more field paths.
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
	#' @concept District
	#' @return A list of StateDistrictMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateDistrictMNS <- function(searchConditionsList = NULL, StateDistrictMNID = F, Code = F, Name = F, StateDistrictTypeCodeMNID = F, CodeName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "StateDistrictMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateDistrictMN
	#'
	#' This function returns a dataframe or json object of a StateDistrictMN
	#' @param StateDistrictMNID The ID of the StateDistrictMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateDistrictMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateDistrictMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateDistrictMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of StateDistrictMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateDistrictMN <- function(StateDistrictMNID, Code = F, Name = F, StateDistrictTypeCodeMNID = F, CodeName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateDistrictMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "StateDistrictMN", objectId = StateDistrictMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateDistrictMN
	#'
	#' This function deletes a StateDistrictMN
	#' @param StateDistrictMNID The ID of the StateDistrictMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The StateDistrictMNID of the deleted StateDistrictMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateDistrictMN <- function(StateDistrictMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "StateDistrictMN", objectId = StateDistrictMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateDistrictMN
	#'
	#' This function creates a StateDistrictMN
	#' @param fieldNames The field values to give the created StateDistrictMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created StateDistrictMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateDistrictMN <- function(Code = NULL, Name = NULL, StateDistrictTypeCodeMNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "StateDistrictMN", body = list(DataObject = body), searchFields = append("StateDistrictMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateDistrictMN
	#'
	#' This function modifies a StateDistrictMN
	#' @param fieldNames The field values to give the modified StateDistrictMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified StateDistrictMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateDistrictMN <- function(StateDistrictMNID, Code = NULL, Name = NULL, StateDistrictTypeCodeMNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "StateDistrictMN", objectId = StateDistrictMNID, body = list(DataObject = body), searchFields = append("StateDistrictMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List RoomTypes
	#'
	#' This function returns a dataframe or json object of RoomTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoomTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoomTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoomType') to get more field paths.
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
	#' @concept District
	#' @return A list of RoomTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRoomTypes <- function(searchConditionsList = NULL, RoomTypeID = F, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EdFiEducationalEnvironmentDescriptorID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "RoomType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RoomType
	#'
	#' This function returns a dataframe or json object of a RoomType
	#' @param RoomTypeID The ID of the RoomType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoomType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoomType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoomType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of RoomType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRoomType <- function(RoomTypeID, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EdFiEducationalEnvironmentDescriptorID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RoomTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "RoomType", objectId = RoomTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RoomType
	#'
	#' This function deletes a RoomType
	#' @param RoomTypeID The ID of the RoomType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The RoomTypeID of the deleted RoomType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRoomType <- function(RoomTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "RoomType", objectId = RoomTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RoomType
	#'
	#' This function creates a RoomType
	#' @param fieldNames The field values to give the created RoomType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created RoomType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRoomType <- function(Code = NULL, Description = NULL, DistrictID = NULL, EdFiEducationalEnvironmentDescriptorID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "RoomType", body = list(DataObject = body), searchFields = append("RoomTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RoomType
	#'
	#' This function modifies a RoomType
	#' @param fieldNames The field values to give the modified RoomType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified RoomType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRoomType <- function(RoomTypeID, Code = NULL, Description = NULL, DistrictID = NULL, EdFiEducationalEnvironmentDescriptorID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "RoomType", objectId = RoomTypeID, body = list(DataObject = body), searchFields = append("RoomTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DistrictConfigEntityYears
	#'
	#' This function returns a dataframe or json object of DistrictConfigEntityYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DistrictConfigEntityYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DistrictConfigEntityYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DistrictConfigEntityYear') to get more field paths.
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
	#' @concept District
	#' @return A list of DistrictConfigEntityYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDistrictConfigEntityYears <- function(searchConditionsList = NULL, ConfigEntityYearID = F, EntityID = F, SchoolYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ConfigEntityYearIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "ConfigEntityYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DistrictConfigEntityYear
	#'
	#' This function returns a dataframe or json object of a DistrictConfigEntityYear
	#' @param DistrictConfigEntityYearID The ID of the DistrictConfigEntityYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DistrictConfigEntityYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DistrictConfigEntityYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DistrictConfigEntityYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of DistrictConfigEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDistrictConfigEntityYear <- function(DistrictConfigEntityYearID, ConfigEntityYearID = F, EntityID = F, SchoolYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ConfigEntityYearIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DistrictConfigEntityYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "ConfigEntityYear", objectId = DistrictConfigEntityYearID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DistrictConfigEntityYear
	#'
	#' This function deletes a DistrictConfigEntityYear
	#' @param DistrictConfigEntityYearID The ID of the DistrictConfigEntityYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The DistrictConfigEntityYearID of the deleted DistrictConfigEntityYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDistrictConfigEntityYear <- function(DistrictConfigEntityYearID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "ConfigEntityYear", objectId = DistrictConfigEntityYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DistrictConfigEntityYear
	#'
	#' This function creates a DistrictConfigEntityYear
	#' @param fieldNames The field values to give the created DistrictConfigEntityYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created DistrictConfigEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDistrictConfigEntityYear <- function(EntityID = NULL, SchoolYearID = NULL, ConfigEntityYearIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "ConfigEntityYear", body = list(DataObject = body), searchFields = append("ConfigEntityYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DistrictConfigEntityYear
	#'
	#' This function modifies a DistrictConfigEntityYear
	#' @param fieldNames The field values to give the modified DistrictConfigEntityYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified DistrictConfigEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDistrictConfigEntityYear <- function(ConfigEntityYearID, EntityID = NULL, SchoolYearID = NULL, ConfigEntityYearIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "ConfigEntityYear", objectId = ConfigEntityYearID, body = list(DataObject = body), searchFields = append("ConfigEntityYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FiscalYears
	#'
	#' This function returns a dataframe or json object of FiscalYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FiscalYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FiscalYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FiscalYear') to get more field paths.
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
	#' @concept District
	#' @return A list of FiscalYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFiscalYears <- function(searchConditionsList = NULL, FiscalYearID = F, Description = F, StartDate = F, EndDate = F, DistrictID = F, IsLockedByHR = F, ConflictCashReceiptDeposits = F, ConflictJournalEntries = F, ConflictAccountsPayableRuns = F, ConflictBudgetAmendments = F, ConflictPayrollRuns = F, ConflictInvoices = F, ConflictPurchaseOrders = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ConflictAccountingUpdates = F, ConflictDepreciations = F, ConflictAdditionDisposals = F, NumericYear = F, ConflictWarehouseRequests = F, IsClosed = F, DynamicRelationshipID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "FiscalYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FiscalYear
	#'
	#' This function returns a dataframe or json object of a FiscalYear
	#' @param FiscalYearID The ID of the FiscalYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FiscalYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FiscalYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FiscalYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of FiscalYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFiscalYear <- function(FiscalYearID, Description = F, StartDate = F, EndDate = F, DistrictID = F, IsLockedByHR = F, ConflictCashReceiptDeposits = F, ConflictJournalEntries = F, ConflictAccountsPayableRuns = F, ConflictBudgetAmendments = F, ConflictPayrollRuns = F, ConflictInvoices = F, ConflictPurchaseOrders = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ConflictAccountingUpdates = F, ConflictDepreciations = F, ConflictAdditionDisposals = F, NumericYear = F, ConflictWarehouseRequests = F, IsClosed = F, DynamicRelationshipID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FiscalYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "FiscalYear", objectId = FiscalYearID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FiscalYear
	#'
	#' This function deletes a FiscalYear
	#' @param FiscalYearID The ID of the FiscalYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The FiscalYearID of the deleted FiscalYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFiscalYear <- function(FiscalYearID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "FiscalYear", objectId = FiscalYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FiscalYear
	#'
	#' This function creates a FiscalYear
	#' @param fieldNames The field values to give the created FiscalYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created FiscalYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFiscalYear <- function(Description = NULL, StartDate = NULL, EndDate = NULL, DistrictID = NULL, IsLockedByHR = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "FiscalYear", body = list(DataObject = body), searchFields = append("FiscalYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FiscalYear
	#'
	#' This function modifies a FiscalYear
	#' @param fieldNames The field values to give the modified FiscalYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified FiscalYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFiscalYear <- function(FiscalYearID, Description = NULL, StartDate = NULL, EndDate = NULL, DistrictID = NULL, IsLockedByHR = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "FiscalYear", objectId = FiscalYearID, body = list(DataObject = body), searchFields = append("FiscalYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DistrictGroups
	#'
	#' This function returns a dataframe or json object of DistrictGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DistrictGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DistrictGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DistrictGroup') to get more field paths.
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
	#' @concept District
	#' @return A list of DistrictGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDistrictGroups <- function(searchConditionsList = NULL, DistrictGroupID = F, Name = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "DistrictGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DistrictGroup
	#'
	#' This function returns a dataframe or json object of a DistrictGroup
	#' @param DistrictGroupID The ID of the DistrictGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DistrictGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DistrictGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DistrictGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of DistrictGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDistrictGroup <- function(DistrictGroupID, Name = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DistrictGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "DistrictGroup", objectId = DistrictGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DistrictGroup
	#'
	#' This function deletes a DistrictGroup
	#' @param DistrictGroupID The ID of the DistrictGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The DistrictGroupID of the deleted DistrictGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDistrictGroup <- function(DistrictGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "DistrictGroup", objectId = DistrictGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DistrictGroup
	#'
	#' This function creates a DistrictGroup
	#' @param fieldNames The field values to give the created DistrictGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created DistrictGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDistrictGroup <- function(Name = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "DistrictGroup", body = list(DataObject = body), searchFields = append("DistrictGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DistrictGroup
	#'
	#' This function modifies a DistrictGroup
	#' @param fieldNames The field values to give the modified DistrictGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified DistrictGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDistrictGroup <- function(DistrictGroupID, Name = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "DistrictGroup", objectId = DistrictGroupID, body = list(DataObject = body), searchFields = append("DistrictGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntityGroups
	#'
	#' This function returns a dataframe or json object of EntityGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityGroup') to get more field paths.
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
	#' @concept District
	#' @return A list of EntityGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntityGroups <- function(searchConditionsList = NULL, EntityGroupID = F, DistrictID = F, Name = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "EntityGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntityGroup
	#'
	#' This function returns a dataframe or json object of an EntityGroup
	#' @param EntityGroupID The ID of the EntityGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of EntityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntityGroup <- function(EntityGroupID, DistrictID = F, Name = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntityGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "EntityGroup", objectId = EntityGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntityGroup
	#'
	#' This function deletes an EntityGroup
	#' @param EntityGroupID The ID of the EntityGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The EntityGroupID of the deleted EntityGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntityGroup <- function(EntityGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "EntityGroup", objectId = EntityGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntityGroup
	#'
	#' This function creates an EntityGroup
	#' @param fieldNames The field values to give the created EntityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created EntityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntityGroup <- function(DistrictID = NULL, Name = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "EntityGroup", body = list(DataObject = body), searchFields = append("EntityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntityGroup
	#'
	#' This function modifies an EntityGroup
	#' @param fieldNames The field values to give the modified EntityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified EntityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntityGroup <- function(EntityGroupID, DistrictID = NULL, Name = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "EntityGroup", objectId = EntityGroupID, body = list(DataObject = body), searchFields = append("EntityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntityGroupEntities
	#'
	#' This function returns a dataframe or json object of EntityGroupEntities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityGroupEntities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityGroupEntities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityGroupEntity') to get more field paths.
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
	#' @concept District
	#' @return A list of EntityGroupEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntityGroupEntities <- function(searchConditionsList = NULL, EntityGroupEntityID = F, EntityGroupID = F, EntityID = F, SchoolYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "EntityGroupEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntityGroupEntity
	#'
	#' This function returns a dataframe or json object of an EntityGroupEntity
	#' @param EntityGroupEntityID The ID of the EntityGroupEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityGroupEntity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityGroupEntity.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityGroupEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of EntityGroupEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntityGroupEntity <- function(EntityGroupEntityID, EntityGroupID = F, EntityID = F, SchoolYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntityGroupEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "EntityGroupEntity", objectId = EntityGroupEntityID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntityGroupEntity
	#'
	#' This function deletes an EntityGroupEntity
	#' @param EntityGroupEntityID The ID of the EntityGroupEntity to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The EntityGroupEntityID of the deleted EntityGroupEntity.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntityGroupEntity <- function(EntityGroupEntityID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "EntityGroupEntity", objectId = EntityGroupEntityID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntityGroupEntity
	#'
	#' This function creates an EntityGroupEntity
	#' @param fieldNames The field values to give the created EntityGroupEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created EntityGroupEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntityGroupEntity <- function(EntityGroupID = NULL, EntityID = NULL, SchoolYearID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "EntityGroupEntity", body = list(DataObject = body), searchFields = append("EntityGroupEntityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntityGroupEntity
	#'
	#' This function modifies an EntityGroupEntity
	#' @param fieldNames The field values to give the modified EntityGroupEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified EntityGroupEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntityGroupEntity <- function(EntityGroupEntityID, EntityGroupID = NULL, EntityID = NULL, SchoolYearID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "EntityGroupEntity", objectId = EntityGroupEntityID, body = list(DataObject = body), searchFields = append("EntityGroupEntityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Rooms
	#'
	#' This function returns a dataframe or json object of Rooms
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Rooms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Rooms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Room') to get more field paths.
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
	#' @concept District
	#' @return A list of Rooms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRooms <- function(searchConditionsList = NULL, RoomID = F, RoomNumber = F, BuildingID = F, Description = F, MaxConcurrentSections = F, PhoneNumber = F, PhoneExtension = F, PhoneNumberIsInternational = F, MaxSeats = F, SquareFootage = F, FormattedPhoneNumber = F, RoomNumberDescription = F, RoomTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BuildingCodeRoomNumber = F, SeatsAvailableForDateRangeAndDisplayPeriods = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "Room", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Room
	#'
	#' This function returns a dataframe or json object of a Room
	#' @param RoomID The ID of the Room to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Room. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Room.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Room') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of Room
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRoom <- function(RoomID, RoomNumber = F, BuildingID = F, Description = F, MaxConcurrentSections = F, PhoneNumber = F, PhoneExtension = F, PhoneNumberIsInternational = F, MaxSeats = F, SquareFootage = F, FormattedPhoneNumber = F, RoomNumberDescription = F, RoomTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BuildingCodeRoomNumber = F, SeatsAvailableForDateRangeAndDisplayPeriods = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RoomID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "Room", objectId = RoomID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Room
	#'
	#' This function deletes a Room
	#' @param RoomID The ID of the Room to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The RoomID of the deleted Room.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRoom <- function(RoomID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "Room", objectId = RoomID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Room
	#'
	#' This function creates a Room
	#' @param fieldNames The field values to give the created Room. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created Room
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRoom <- function(RoomNumber = NULL, BuildingID = NULL, Description = NULL, MaxConcurrentSections = NULL, PhoneNumber = NULL, PhoneExtension = NULL, PhoneNumberIsInternational = NULL, MaxSeats = NULL, SquareFootage = NULL, RoomTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "Room", body = list(DataObject = body), searchFields = append("RoomID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Room
	#'
	#' This function modifies a Room
	#' @param fieldNames The field values to give the modified Room. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified Room
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRoom <- function(RoomID, RoomNumber = NULL, BuildingID = NULL, Description = NULL, MaxConcurrentSections = NULL, PhoneNumber = NULL, PhoneExtension = NULL, PhoneNumberIsInternational = NULL, MaxSeats = NULL, SquareFootage = NULL, RoomTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "Room", objectId = RoomID, body = list(DataObject = body), searchFields = append("RoomID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchoolYears
	#'
	#' This function returns a dataframe or json object of SchoolYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolYear') to get more field paths.
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
	#' @concept District
	#' @return A list of SchoolYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchoolYears <- function(searchConditionsList = NULL, SchoolYearID = F, NumericYear = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsCurrentYearForProvidedEntity = F, IsUpcomingYearForProvidedEntity = F, NextNumericYear = F, StudentAwardID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "SchoolYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SchoolYear
	#'
	#' This function returns a dataframe or json object of a SchoolYear
	#' @param SchoolYearID The ID of the SchoolYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of SchoolYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchoolYear <- function(SchoolYearID, NumericYear = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsCurrentYearForProvidedEntity = F, IsUpcomingYearForProvidedEntity = F, NextNumericYear = F, StudentAwardID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchoolYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "SchoolYear", objectId = SchoolYearID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SchoolYear
	#'
	#' This function deletes a SchoolYear
	#' @param SchoolYearID The ID of the SchoolYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The SchoolYearID of the deleted SchoolYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSchoolYear <- function(SchoolYearID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "SchoolYear", objectId = SchoolYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SchoolYear
	#'
	#' This function creates a SchoolYear
	#' @param fieldNames The field values to give the created SchoolYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created SchoolYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSchoolYear <- function(NumericYear = NULL, Description = NULL, StudentAwardID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "SchoolYear", body = list(DataObject = body), searchFields = append("SchoolYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SchoolYear
	#'
	#' This function modifies a SchoolYear
	#' @param fieldNames The field values to give the modified SchoolYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified SchoolYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySchoolYear <- function(SchoolYearID, NumericYear = NULL, Description = NULL, StudentAwardID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "SchoolYear", objectId = SchoolYearID, body = list(DataObject = body), searchFields = append("SchoolYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Entities
	#'
	#' This function returns a dataframe or json object of Entities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Entities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Entities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Entity') to get more field paths.
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
	#' @concept District
	#' @return A list of Entities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntities <- function(searchConditionsList = NULL, EntityID = F, DistrictID = F, SchoolYearIDCurrent = F, Name = F, IsDistrictWide = F, IsSystemWide = F, Code = F, CodeName = F, CampusID = F, ExternalLinkEntityCount = F, TotalPlanEntityYears = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityGroupCount = F, EntityMNID = F, ReportToState = F, EntityCodeOrCombinedCodesFollettExport = F, DistrictCodeEntityCode = F, EnforceAddressRangeDefaults = F, IsCurrentlySelected = F, IlluminateSiteTypeOverride = F, EdFiSchoolID = F, AllowDualEnrollment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "Entity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Entity
	#'
	#' This function returns a dataframe or json object of an Entity
	#' @param EntityID The ID of the Entity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Entity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Entity.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Entity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of Entity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntity <- function(EntityID, DistrictID = F, SchoolYearIDCurrent = F, Name = F, IsDistrictWide = F, IsSystemWide = F, Code = F, CodeName = F, CampusID = F, ExternalLinkEntityCount = F, TotalPlanEntityYears = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityGroupCount = F, EntityMNID = F, ReportToState = F, EntityCodeOrCombinedCodesFollettExport = F, DistrictCodeEntityCode = F, EnforceAddressRangeDefaults = F, IsCurrentlySelected = F, IlluminateSiteTypeOverride = F, EdFiSchoolID = F, AllowDualEnrollment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "Entity", objectId = EntityID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Entity
	#'
	#' This function deletes an Entity
	#' @param EntityID The ID of the Entity to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The EntityID of the deleted Entity.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntity <- function(EntityID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "Entity", objectId = EntityID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Entity
	#'
	#' This function creates an Entity
	#' @param fieldNames The field values to give the created Entity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created Entity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntity <- function(DistrictID = NULL, SchoolYearIDCurrent = NULL, Name = NULL, IsDistrictWide = NULL, IsSystemWide = NULL, Code = NULL, ReportToState = NULL, EnforceAddressRangeDefaults = NULL, IlluminateSiteTypeOverride = NULL, EdFiSchoolID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "Entity", body = list(DataObject = body), searchFields = append("EntityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Entity
	#'
	#' This function modifies an Entity
	#' @param fieldNames The field values to give the modified Entity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified Entity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntity <- function(EntityID, DistrictID = NULL, SchoolYearIDCurrent = NULL, Name = NULL, IsDistrictWide = NULL, IsSystemWide = NULL, Code = NULL, ReportToState = NULL, EnforceAddressRangeDefaults = NULL, IlluminateSiteTypeOverride = NULL, EdFiSchoolID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "Entity", objectId = EntityID, body = list(DataObject = body), searchFields = append("EntityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntityGroupSetupEntities
	#'
	#' This function returns a dataframe or json object of EntityGroupSetupEntities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityGroupSetupEntities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityGroupSetupEntities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityGroupSetupEntity') to get more field paths.
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
	#' @concept District
	#' @return A list of EntityGroupSetupEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntityGroupSetupEntities <- function(searchConditionsList = NULL, EntityGroupSetupEntityID = F, EntityID = F, EntityGroupSetupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "EntityGroupSetupEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntityGroupSetupEntity
	#'
	#' This function returns a dataframe or json object of an EntityGroupSetupEntity
	#' @param EntityGroupSetupEntityID The ID of the EntityGroupSetupEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityGroupSetupEntity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityGroupSetupEntity.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityGroupSetupEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of EntityGroupSetupEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntityGroupSetupEntity <- function(EntityGroupSetupEntityID, EntityID = F, EntityGroupSetupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntityGroupSetupEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "EntityGroupSetupEntity", objectId = EntityGroupSetupEntityID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntityGroupSetupEntity
	#'
	#' This function deletes an EntityGroupSetupEntity
	#' @param EntityGroupSetupEntityID The ID of the EntityGroupSetupEntity to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The EntityGroupSetupEntityID of the deleted EntityGroupSetupEntity.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntityGroupSetupEntity <- function(EntityGroupSetupEntityID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "EntityGroupSetupEntity", objectId = EntityGroupSetupEntityID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntityGroupSetupEntity
	#'
	#' This function creates an EntityGroupSetupEntity
	#' @param fieldNames The field values to give the created EntityGroupSetupEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created EntityGroupSetupEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntityGroupSetupEntity <- function(EntityID = NULL, EntityGroupSetupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "EntityGroupSetupEntity", body = list(DataObject = body), searchFields = append("EntityGroupSetupEntityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntityGroupSetupEntity
	#'
	#' This function modifies an EntityGroupSetupEntity
	#' @param fieldNames The field values to give the modified EntityGroupSetupEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified EntityGroupSetupEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntityGroupSetupEntity <- function(EntityGroupSetupEntityID, EntityID = NULL, EntityGroupSetupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "EntityGroupSetupEntity", objectId = EntityGroupSetupEntityID, body = list(DataObject = body), searchFields = append("EntityGroupSetupEntityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntityGroupSetups
	#'
	#' This function returns a dataframe or json object of EntityGroupSetups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityGroupSetups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityGroupSetups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityGroupSetup') to get more field paths.
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
	#' @concept District
	#' @return A list of EntityGroupSetups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntityGroupSetups <- function(searchConditionsList = NULL, EntityGroupSetupID = F, HasBeenProcessed = F, EntityIDPrimary = F, EntityGroupID = F, SchoolYearID = F, NewGroupName = F, EffectiveGroupName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "EntityGroupSetup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntityGroupSetup
	#'
	#' This function returns a dataframe or json object of an EntityGroupSetup
	#' @param EntityGroupSetupID The ID of the EntityGroupSetup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityGroupSetup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityGroupSetup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityGroupSetup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of EntityGroupSetup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntityGroupSetup <- function(EntityGroupSetupID, HasBeenProcessed = F, EntityIDPrimary = F, EntityGroupID = F, SchoolYearID = F, NewGroupName = F, EffectiveGroupName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntityGroupSetupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "EntityGroupSetup", objectId = EntityGroupSetupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntityGroupSetup
	#'
	#' This function deletes an EntityGroupSetup
	#' @param EntityGroupSetupID The ID of the EntityGroupSetup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The EntityGroupSetupID of the deleted EntityGroupSetup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntityGroupSetup <- function(EntityGroupSetupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "EntityGroupSetup", objectId = EntityGroupSetupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntityGroupSetup
	#'
	#' This function creates an EntityGroupSetup
	#' @param fieldNames The field values to give the created EntityGroupSetup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created EntityGroupSetup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntityGroupSetup <- function(EntityIDPrimary = NULL, EntityGroupID = NULL, SchoolYearID = NULL, NewGroupName = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "EntityGroupSetup", body = list(DataObject = body), searchFields = append("EntityGroupSetupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntityGroupSetup
	#'
	#' This function modifies an EntityGroupSetup
	#' @param fieldNames The field values to give the modified EntityGroupSetup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified EntityGroupSetup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntityGroupSetup <- function(EntityGroupSetupID, EntityIDPrimary = NULL, EntityGroupID = NULL, SchoolYearID = NULL, NewGroupName = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "EntityGroupSetup", objectId = EntityGroupSetupID, body = list(DataObject = body), searchFields = append("EntityGroupSetupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntityGroupSetupRuns
	#'
	#' This function returns a dataframe or json object of EntityGroupSetupRuns
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityGroupSetupRuns. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityGroupSetupRuns.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityGroupSetupRun') to get more field paths.
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
	#' @concept District
	#' @return A list of EntityGroupSetupRuns
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntityGroupSetupRuns <- function(searchConditionsList = NULL, EntityGroupSetupRunID = F, EntityGroupSetupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "EntityGroupSetupRun", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntityGroupSetupRun
	#'
	#' This function returns a dataframe or json object of an EntityGroupSetupRun
	#' @param EntityGroupSetupRunID The ID of the EntityGroupSetupRun to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityGroupSetupRun. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityGroupSetupRun.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityGroupSetupRun') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of EntityGroupSetupRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntityGroupSetupRun <- function(EntityGroupSetupRunID, EntityGroupSetupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntityGroupSetupRunID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "EntityGroupSetupRun", objectId = EntityGroupSetupRunID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntityGroupSetupRun
	#'
	#' This function deletes an EntityGroupSetupRun
	#' @param EntityGroupSetupRunID The ID of the EntityGroupSetupRun to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The EntityGroupSetupRunID of the deleted EntityGroupSetupRun.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntityGroupSetupRun <- function(EntityGroupSetupRunID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "EntityGroupSetupRun", objectId = EntityGroupSetupRunID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntityGroupSetupRun
	#'
	#' This function creates an EntityGroupSetupRun
	#' @param fieldNames The field values to give the created EntityGroupSetupRun. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created EntityGroupSetupRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntityGroupSetupRun <- function(EntityGroupSetupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "EntityGroupSetupRun", body = list(DataObject = body), searchFields = append("EntityGroupSetupRunID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntityGroupSetupRun
	#'
	#' This function modifies an EntityGroupSetupRun
	#' @param fieldNames The field values to give the modified EntityGroupSetupRun. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified EntityGroupSetupRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntityGroupSetupRun <- function(EntityGroupSetupRunID, EntityGroupSetupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "EntityGroupSetupRun", objectId = EntityGroupSetupRunID, body = list(DataObject = body), searchFields = append("EntityGroupSetupRunID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntityGroupSetupRunDetails
	#'
	#' This function returns a dataframe or json object of EntityGroupSetupRunDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityGroupSetupRunDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityGroupSetupRunDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityGroupSetupRunDetail') to get more field paths.
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
	#' @concept District
	#' @return A list of EntityGroupSetupRunDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntityGroupSetupRunDetails <- function(searchConditionsList = NULL, EntityGroupSetupRunDetailID = F, EntityGroupSetupRunID = F, Module = F, Object = F, ObjectPrimaryKey = F, EntityID = F, SchoolYearID = F, IsProcessed = F, IsUpdated = F, ChangeType = F, IdentifyingFields = F, OriginalValues = F, NewValues = F, Error = F, EntityGroupKey = F, NewFieldValues = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "EntityGroupSetupRunDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntityGroupSetupRunDetail
	#'
	#' This function returns a dataframe or json object of an EntityGroupSetupRunDetail
	#' @param EntityGroupSetupRunDetailID The ID of the EntityGroupSetupRunDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityGroupSetupRunDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityGroupSetupRunDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityGroupSetupRunDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of EntityGroupSetupRunDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntityGroupSetupRunDetail <- function(EntityGroupSetupRunDetailID, EntityGroupSetupRunID = F, Module = F, Object = F, ObjectPrimaryKey = F, EntityID = F, SchoolYearID = F, IsProcessed = F, IsUpdated = F, ChangeType = F, IdentifyingFields = F, OriginalValues = F, NewValues = F, Error = F, EntityGroupKey = F, NewFieldValues = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntityGroupSetupRunDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "EntityGroupSetupRunDetail", objectId = EntityGroupSetupRunDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntityGroupSetupRunDetail
	#'
	#' This function deletes an EntityGroupSetupRunDetail
	#' @param EntityGroupSetupRunDetailID The ID of the EntityGroupSetupRunDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The EntityGroupSetupRunDetailID of the deleted EntityGroupSetupRunDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntityGroupSetupRunDetail <- function(EntityGroupSetupRunDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "EntityGroupSetupRunDetail", objectId = EntityGroupSetupRunDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntityGroupSetupRunDetail
	#'
	#' This function creates an EntityGroupSetupRunDetail
	#' @param fieldNames The field values to give the created EntityGroupSetupRunDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created EntityGroupSetupRunDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntityGroupSetupRunDetail <- function(EntityGroupSetupRunID = NULL, Module = NULL, Object = NULL, ObjectPrimaryKey = NULL, EntityID = NULL, SchoolYearID = NULL, ChangeType = NULL, IdentifyingFields = NULL, OriginalValues = NULL, NewValues = NULL, Error = NULL, EntityGroupKey = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "EntityGroupSetupRunDetail", body = list(DataObject = body), searchFields = append("EntityGroupSetupRunDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntityGroupSetupRunDetail
	#'
	#' This function modifies an EntityGroupSetupRunDetail
	#' @param fieldNames The field values to give the modified EntityGroupSetupRunDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified EntityGroupSetupRunDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntityGroupSetupRunDetail <- function(EntityGroupSetupRunDetailID, EntityGroupSetupRunID = NULL, Module = NULL, Object = NULL, ObjectPrimaryKey = NULL, EntityID = NULL, SchoolYearID = NULL, ChangeType = NULL, IdentifyingFields = NULL, OriginalValues = NULL, NewValues = NULL, Error = NULL, EntityGroupKey = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "EntityGroupSetupRunDetail", objectId = EntityGroupSetupRunDetailID, body = list(DataObject = body), searchFields = append("EntityGroupSetupRunDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalendarYears
	#'
	#' This function returns a dataframe or json object of CalendarYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarYear') to get more field paths.
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
	#' @concept District
	#' @return A list of CalendarYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalendarYears <- function(searchConditionsList = NULL, CalendarYearID = F, NumericYear = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "CalendarYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CalendarYear
	#'
	#' This function returns a dataframe or json object of a CalendarYear
	#' @param CalendarYearID The ID of the CalendarYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of CalendarYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalendarYear <- function(CalendarYearID, NumericYear = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalendarYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "CalendarYear", objectId = CalendarYearID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CalendarYear
	#'
	#' This function deletes a CalendarYear
	#' @param CalendarYearID The ID of the CalendarYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The CalendarYearID of the deleted CalendarYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCalendarYear <- function(CalendarYearID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "CalendarYear", objectId = CalendarYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CalendarYear
	#'
	#' This function creates a CalendarYear
	#' @param fieldNames The field values to give the created CalendarYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created CalendarYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCalendarYear <- function(NumericYear = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "CalendarYear", body = list(DataObject = body), searchFields = append("CalendarYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CalendarYear
	#'
	#' This function modifies a CalendarYear
	#' @param fieldNames The field values to give the modified CalendarYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified CalendarYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCalendarYear <- function(CalendarYearID, NumericYear = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "CalendarYear", objectId = CalendarYearID, body = list(DataObject = body), searchFields = append("CalendarYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntityCloneDestinations
	#'
	#' This function returns a dataframe or json object of EntityCloneDestinations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityCloneDestinations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityCloneDestinations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityCloneDestination') to get more field paths.
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
	#' @concept District
	#' @return A list of EntityCloneDestinations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntityCloneDestinations <- function(searchConditionsList = NULL, EntityCloneDestinationID = F, EntityCloneRunID = F, EntityID = F, SchoolYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "EntityCloneDestination", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntityCloneDestination
	#'
	#' This function returns a dataframe or json object of an EntityCloneDestination
	#' @param EntityCloneDestinationID The ID of the EntityCloneDestination to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityCloneDestination. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityCloneDestination.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityCloneDestination') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of EntityCloneDestination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntityCloneDestination <- function(EntityCloneDestinationID, EntityCloneRunID = F, EntityID = F, SchoolYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntityCloneDestinationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "EntityCloneDestination", objectId = EntityCloneDestinationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntityCloneDestination
	#'
	#' This function deletes an EntityCloneDestination
	#' @param EntityCloneDestinationID The ID of the EntityCloneDestination to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The EntityCloneDestinationID of the deleted EntityCloneDestination.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntityCloneDestination <- function(EntityCloneDestinationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "EntityCloneDestination", objectId = EntityCloneDestinationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntityCloneDestination
	#'
	#' This function creates an EntityCloneDestination
	#' @param fieldNames The field values to give the created EntityCloneDestination. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created EntityCloneDestination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntityCloneDestination <- function(EntityCloneRunID = NULL, EntityID = NULL, SchoolYearID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "EntityCloneDestination", body = list(DataObject = body), searchFields = append("EntityCloneDestinationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntityCloneDestination
	#'
	#' This function modifies an EntityCloneDestination
	#' @param fieldNames The field values to give the modified EntityCloneDestination. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified EntityCloneDestination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntityCloneDestination <- function(EntityCloneDestinationID, EntityCloneRunID = NULL, EntityID = NULL, SchoolYearID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "EntityCloneDestination", objectId = EntityCloneDestinationID, body = list(DataObject = body), searchFields = append("EntityCloneDestinationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntityCloneErrors
	#'
	#' This function returns a dataframe or json object of EntityCloneErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityCloneErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityCloneErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityCloneError') to get more field paths.
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
	#' @concept District
	#' @return A list of EntityCloneErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntityCloneErrors <- function(searchConditionsList = NULL, EntityCloneErrorID = F, EntityCloneSelectedObjectID = F, Message = F, ObjectJSON = F, AttemptedOperation = F, EntityIDTarget = F, SchoolYearIDTarget = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "EntityCloneError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntityCloneError
	#'
	#' This function returns a dataframe or json object of an EntityCloneError
	#' @param EntityCloneErrorID The ID of the EntityCloneError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityCloneError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityCloneError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityCloneError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of EntityCloneError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntityCloneError <- function(EntityCloneErrorID, EntityCloneSelectedObjectID = F, Message = F, ObjectJSON = F, AttemptedOperation = F, EntityIDTarget = F, SchoolYearIDTarget = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntityCloneErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "EntityCloneError", objectId = EntityCloneErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntityCloneError
	#'
	#' This function deletes an EntityCloneError
	#' @param EntityCloneErrorID The ID of the EntityCloneError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The EntityCloneErrorID of the deleted EntityCloneError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntityCloneError <- function(EntityCloneErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "EntityCloneError", objectId = EntityCloneErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntityCloneError
	#'
	#' This function creates an EntityCloneError
	#' @param fieldNames The field values to give the created EntityCloneError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created EntityCloneError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntityCloneError <- function(EntityCloneSelectedObjectID = NULL, Message = NULL, ObjectJSON = NULL, AttemptedOperation = NULL, EntityIDTarget = NULL, SchoolYearIDTarget = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "EntityCloneError", body = list(DataObject = body), searchFields = append("EntityCloneErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntityCloneError
	#'
	#' This function modifies an EntityCloneError
	#' @param fieldNames The field values to give the modified EntityCloneError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified EntityCloneError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntityCloneError <- function(EntityCloneErrorID, EntityCloneSelectedObjectID = NULL, Message = NULL, ObjectJSON = NULL, AttemptedOperation = NULL, EntityIDTarget = NULL, SchoolYearIDTarget = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "EntityCloneError", objectId = EntityCloneErrorID, body = list(DataObject = body), searchFields = append("EntityCloneErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntityCloneRuns
	#'
	#' This function returns a dataframe or json object of EntityCloneRuns
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityCloneRuns. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityCloneRuns.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityCloneRun') to get more field paths.
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
	#' @concept District
	#' @return A list of EntityCloneRuns
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntityCloneRuns <- function(searchConditionsList = NULL, EntityCloneRunID = F, Status = F, TotalRecordsAdded = F, TotalRecordsUpdated = F, TotalRecordsDeleted = F, TotalRecordsErrored = F, TargetEntities = F, TargetYears = F, EntityIDSource = F, SchoolYearIDSource = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "EntityCloneRun", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntityCloneRun
	#'
	#' This function returns a dataframe or json object of an EntityCloneRun
	#' @param EntityCloneRunID The ID of the EntityCloneRun to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityCloneRun. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityCloneRun.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityCloneRun') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of EntityCloneRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntityCloneRun <- function(EntityCloneRunID, Status = F, TotalRecordsAdded = F, TotalRecordsUpdated = F, TotalRecordsDeleted = F, TotalRecordsErrored = F, TargetEntities = F, TargetYears = F, EntityIDSource = F, SchoolYearIDSource = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntityCloneRunID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "EntityCloneRun", objectId = EntityCloneRunID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntityCloneRun
	#'
	#' This function deletes an EntityCloneRun
	#' @param EntityCloneRunID The ID of the EntityCloneRun to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The EntityCloneRunID of the deleted EntityCloneRun.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntityCloneRun <- function(EntityCloneRunID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "EntityCloneRun", objectId = EntityCloneRunID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntityCloneRun
	#'
	#' This function creates an EntityCloneRun
	#' @param fieldNames The field values to give the created EntityCloneRun. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created EntityCloneRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntityCloneRun <- function(Status = NULL, EntityIDSource = NULL, SchoolYearIDSource = NULL, MediaID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "EntityCloneRun", body = list(DataObject = body), searchFields = append("EntityCloneRunID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntityCloneRun
	#'
	#' This function modifies an EntityCloneRun
	#' @param fieldNames The field values to give the modified EntityCloneRun. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified EntityCloneRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntityCloneRun <- function(EntityCloneRunID, Status = NULL, EntityIDSource = NULL, SchoolYearIDSource = NULL, MediaID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "EntityCloneRun", objectId = EntityCloneRunID, body = list(DataObject = body), searchFields = append("EntityCloneRunID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntityCloneSelectedObjects
	#'
	#' This function returns a dataframe or json object of EntityCloneSelectedObjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityCloneSelectedObjects. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityCloneSelectedObjects.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityCloneSelectedObject') to get more field paths.
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
	#' @concept District
	#' @return A list of EntityCloneSelectedObjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntityCloneSelectedObjects <- function(searchConditionsList = NULL, EntityCloneSelectedObjectID = F, EntityCloneRunID = F, ObjectID = F, IsAdding = F, IsUpdating = F, IsDeleting = F, NumberOfRecordsExported = F, NumberOfRecordsAdded = F, NumberOfRecordsUpdated = F, NumberOfRecordsDeleted = F, NumberOfRecordsErrored = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CloneOrder = F, Dependencies = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "EntityCloneSelectedObject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntityCloneSelectedObject
	#'
	#' This function returns a dataframe or json object of an EntityCloneSelectedObject
	#' @param EntityCloneSelectedObjectID The ID of the EntityCloneSelectedObject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityCloneSelectedObject. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityCloneSelectedObject.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityCloneSelectedObject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of EntityCloneSelectedObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntityCloneSelectedObject <- function(EntityCloneSelectedObjectID, EntityCloneRunID = F, ObjectID = F, IsAdding = F, IsUpdating = F, IsDeleting = F, NumberOfRecordsExported = F, NumberOfRecordsAdded = F, NumberOfRecordsUpdated = F, NumberOfRecordsDeleted = F, NumberOfRecordsErrored = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CloneOrder = F, Dependencies = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntityCloneSelectedObjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "EntityCloneSelectedObject", objectId = EntityCloneSelectedObjectID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntityCloneSelectedObject
	#'
	#' This function deletes an EntityCloneSelectedObject
	#' @param EntityCloneSelectedObjectID The ID of the EntityCloneSelectedObject to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The EntityCloneSelectedObjectID of the deleted EntityCloneSelectedObject.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntityCloneSelectedObject <- function(EntityCloneSelectedObjectID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "EntityCloneSelectedObject", objectId = EntityCloneSelectedObjectID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntityCloneSelectedObject
	#'
	#' This function creates an EntityCloneSelectedObject
	#' @param fieldNames The field values to give the created EntityCloneSelectedObject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created EntityCloneSelectedObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntityCloneSelectedObject <- function(EntityCloneRunID = NULL, ObjectID = NULL, IsAdding = NULL, IsUpdating = NULL, IsDeleting = NULL, NumberOfRecordsExported = NULL, NumberOfRecordsAdded = NULL, NumberOfRecordsUpdated = NULL, NumberOfRecordsDeleted = NULL, NumberOfRecordsErrored = NULL, CloneOrder = NULL, Dependencies = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "EntityCloneSelectedObject", body = list(DataObject = body), searchFields = append("EntityCloneSelectedObjectID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntityCloneSelectedObject
	#'
	#' This function modifies an EntityCloneSelectedObject
	#' @param fieldNames The field values to give the modified EntityCloneSelectedObject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified EntityCloneSelectedObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntityCloneSelectedObject <- function(EntityCloneSelectedObjectID, EntityCloneRunID = NULL, ObjectID = NULL, IsAdding = NULL, IsUpdating = NULL, IsDeleting = NULL, NumberOfRecordsExported = NULL, NumberOfRecordsAdded = NULL, NumberOfRecordsUpdated = NULL, NumberOfRecordsDeleted = NULL, NumberOfRecordsErrored = NULL, CloneOrder = NULL, Dependencies = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "EntityCloneSelectedObject", objectId = EntityCloneSelectedObjectID, body = list(DataObject = body), searchFields = append("EntityCloneSelectedObjectID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntityCloneSelections
	#'
	#' This function returns a dataframe or json object of EntityCloneSelections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityCloneSelections. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityCloneSelections.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityCloneSelection') to get more field paths.
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
	#' @concept District
	#' @return A list of EntityCloneSelections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntityCloneSelections <- function(searchConditionsList = NULL, EntityCloneSelectionID = F, EntityCloneRunID = F, ModuleID = F, ObjectName = F, IsAdding = F, IsUpdating = F, IsDeleting = F, Dependencies = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "EntityCloneSelection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntityCloneSelection
	#'
	#' This function returns a dataframe or json object of an EntityCloneSelection
	#' @param EntityCloneSelectionID The ID of the EntityCloneSelection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityCloneSelection. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityCloneSelection.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityCloneSelection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of EntityCloneSelection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntityCloneSelection <- function(EntityCloneSelectionID, EntityCloneRunID = F, ModuleID = F, ObjectName = F, IsAdding = F, IsUpdating = F, IsDeleting = F, Dependencies = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntityCloneSelectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "EntityCloneSelection", objectId = EntityCloneSelectionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntityCloneSelection
	#'
	#' This function deletes an EntityCloneSelection
	#' @param EntityCloneSelectionID The ID of the EntityCloneSelection to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The EntityCloneSelectionID of the deleted EntityCloneSelection.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntityCloneSelection <- function(EntityCloneSelectionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "EntityCloneSelection", objectId = EntityCloneSelectionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntityCloneSelection
	#'
	#' This function creates an EntityCloneSelection
	#' @param fieldNames The field values to give the created EntityCloneSelection. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created EntityCloneSelection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntityCloneSelection <- function(EntityCloneRunID = NULL, ModuleID = NULL, ObjectName = NULL, IsAdding = NULL, IsUpdating = NULL, IsDeleting = NULL, Dependencies = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "EntityCloneSelection", body = list(DataObject = body), searchFields = append("EntityCloneSelectionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntityCloneSelection
	#'
	#' This function modifies an EntityCloneSelection
	#' @param fieldNames The field values to give the modified EntityCloneSelection. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified EntityCloneSelection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntityCloneSelection <- function(EntityCloneSelectionID, EntityCloneRunID = NULL, ModuleID = NULL, ObjectName = NULL, IsAdding = NULL, IsUpdating = NULL, IsDeleting = NULL, Dependencies = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "EntityCloneSelection", objectId = EntityCloneSelectionID, body = list(DataObject = body), searchFields = append("EntityCloneSelectionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DualEnrollmentEntities
	#'
	#' This function returns a dataframe or json object of DualEnrollmentEntities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DualEnrollmentEntities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DualEnrollmentEntities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DualEnrollmentEntity') to get more field paths.
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
	#' @concept District
	#' @return A list of DualEnrollmentEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDualEnrollmentEntities <- function(searchConditionsList = NULL, DualEnrollmentEntityID = F, EntityIDTarget = F, EntityIDAllowDualEnrollment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "District", objectName = "DualEnrollmentEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DualEnrollmentEntity
	#'
	#' This function returns a dataframe or json object of a DualEnrollmentEntity
	#' @param DualEnrollmentEntityID The ID of the DualEnrollmentEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DualEnrollmentEntity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DualEnrollmentEntity.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DualEnrollmentEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A dataframe or of DualEnrollmentEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDualEnrollmentEntity <- function(DualEnrollmentEntityID, EntityIDTarget = F, EntityIDAllowDualEnrollment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DualEnrollmentEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "District", objectName = "DualEnrollmentEntity", objectId = DualEnrollmentEntityID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DualEnrollmentEntity
	#'
	#' This function deletes a DualEnrollmentEntity
	#' @param DualEnrollmentEntityID The ID of the DualEnrollmentEntity to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The DualEnrollmentEntityID of the deleted DualEnrollmentEntity.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDualEnrollmentEntity <- function(DualEnrollmentEntityID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "District", objectName = "DualEnrollmentEntity", objectId = DualEnrollmentEntityID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DualEnrollmentEntity
	#'
	#' This function creates a DualEnrollmentEntity
	#' @param fieldNames The field values to give the created DualEnrollmentEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return A newly created DualEnrollmentEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDualEnrollmentEntity <- function(EntityIDTarget = NULL, EntityIDAllowDualEnrollment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "District", objectName = "DualEnrollmentEntity", body = list(DataObject = body), searchFields = append("DualEnrollmentEntityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DualEnrollmentEntity
	#'
	#' This function modifies a DualEnrollmentEntity
	#' @param fieldNames The field values to give the modified DualEnrollmentEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept District
	#' @return The modified DualEnrollmentEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDualEnrollmentEntity <- function(DualEnrollmentEntityID, EntityIDTarget = NULL, EntityIDAllowDualEnrollment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "District", objectName = "DualEnrollmentEntity", objectId = DualEnrollmentEntityID, body = list(DataObject = body), searchFields = append("DualEnrollmentEntityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
