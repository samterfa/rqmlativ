
	#' List DemographicsConfigSystems
	#'
	#' This function returns a dataframe or json object of DemographicsConfigSystems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DemographicsConfigSystems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DemographicsConfigSystems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DemographicsConfigSystem') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of DemographicsConfigSystems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDemographicsConfigSystems <- function(searchConditionsList = NULL, ConfigSystemID = F, CreatedTime = F, DefaultCaseAddressType = F, DefaultCaseNameType = F, ModifiedTime = F, NameNumberLength = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UseSyncedNameNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "ConfigSystem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DemographicsConfigSystem
	#'
	#' This function returns a dataframe or json object of a DemographicsConfigSystem
	#' @param DemographicsConfigSystemID The ID of the DemographicsConfigSystem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DemographicsConfigSystem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DemographicsConfigSystem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DemographicsConfigSystem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of DemographicsConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDemographicsConfigSystem <- function(DemographicsConfigSystemID, ConfigSystemID = F, CreatedTime = F, DefaultCaseAddressType = F, DefaultCaseNameType = F, ModifiedTime = F, NameNumberLength = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UseSyncedNameNumber = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DemographicsConfigSystemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "ConfigSystem", objectId = DemographicsConfigSystemID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DemographicsConfigSystem
	#'
	#' This function deletes a DemographicsConfigSystem
	#' @param DemographicsConfigSystemID The ID of the DemographicsConfigSystem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The DemographicsConfigSystemID of the deleted DemographicsConfigSystem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDemographicsConfigSystem <- function(DemographicsConfigSystemID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "ConfigSystem", objectId = DemographicsConfigSystemID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DemographicsConfigSystem
	#'
	#' This function creates a DemographicsConfigSystem
	#' @param fieldNames The field values to give the created DemographicsConfigSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created DemographicsConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDemographicsConfigSystem <- function(DefaultCaseAddressType = NULL, DefaultCaseNameType = NULL, NameNumberLength = NULL, UseSyncedNameNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "ConfigSystem", body = list(DataObject = body), searchFields = append("ConfigSystemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DemographicsConfigSystem
	#'
	#' This function modifies a DemographicsConfigSystem
	#' @param fieldNames The field values to give the modified DemographicsConfigSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified DemographicsConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDemographicsConfigSystem <- function(ConfigSystemID, DefaultCaseAddressType = NULL, DefaultCaseNameType = NULL, NameNumberLength = NULL, UseSyncedNameNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "ConfigSystem", objectId = ConfigSystemID, body = list(DataObject = body), searchFields = append("ConfigSystemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Dependents
	#'
	#' This function returns a dataframe or json object of Dependents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Dependents. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Dependents.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Dependent') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of Dependents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDependents <- function(searchConditionsList = NULL, CreatedTime = F, DependentID = F, ModifiedTime = F, NameIDDependent = F, NameIDOwner = F, RelationshipID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "Dependent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Dependent
	#'
	#' This function returns a dataframe or json object of a Dependent
	#' @param DependentID The ID of the Dependent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Dependent. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Dependent.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Dependent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of Dependent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDependent <- function(DependentID, CreatedTime = F, ModifiedTime = F, NameIDDependent = F, NameIDOwner = F, RelationshipID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DependentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "Dependent", objectId = DependentID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Dependent
	#'
	#' This function deletes a Dependent
	#' @param DependentID The ID of the Dependent to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The DependentID of the deleted Dependent.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDependent <- function(DependentID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "Dependent", objectId = DependentID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Dependent
	#'
	#' This function creates a Dependent
	#' @param fieldNames The field values to give the created Dependent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created Dependent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDependent <- function(NameIDDependent = NULL, NameIDOwner = NULL, RelationshipID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "Dependent", body = list(DataObject = body), searchFields = append("DependentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Dependent
	#'
	#' This function modifies a Dependent
	#' @param fieldNames The field values to give the modified Dependent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified Dependent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDependent <- function(DependentID, NameIDDependent = NULL, NameIDOwner = NULL, RelationshipID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "Dependent", objectId = DependentID, body = list(DataObject = body), searchFields = append("DependentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Employers
	#'
	#' This function returns a dataframe or json object of Employers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Employers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Employers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Employer') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of Employers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployers <- function(searchConditionsList = NULL, ACATransmitterControlCode = F, CreatedTime = F, DistrictID = F, EEO4ControlNumber = F, EEO4JurisdictionName = F, EEO5NameOfCertifyingOfficial = F, EEO5OfficeOfSchoolNumber = F, EEO5SchoolDistrictName = F, EEOCTitleOfCertifyingOfficial = F, EmployerID = F, EmployerMNID = F, FederalEEO4FunctionID = F, ModifiedTime = F, NameID = F, NameIDACAContactPerson = F, NameIDEEO5CertifyingOfficial = F, NameIDW2CContactPerson = F, PERAEmployerNumber = F, TRACountyGroupNumber = F, TRADistrictUnitNumber = F, UnemploymentInsuranceAccountNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "Employer", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Employer
	#'
	#' This function returns a dataframe or json object of an Employer
	#' @param EmployerID The ID of the Employer to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Employer. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Employer.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Employer') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of Employer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployer <- function(EmployerID, ACATransmitterControlCode = F, CreatedTime = F, DistrictID = F, EEO4ControlNumber = F, EEO4JurisdictionName = F, EEO5NameOfCertifyingOfficial = F, EEO5OfficeOfSchoolNumber = F, EEO5SchoolDistrictName = F, EEOCTitleOfCertifyingOfficial = F, EmployerMNID = F, FederalEEO4FunctionID = F, ModifiedTime = F, NameID = F, NameIDACAContactPerson = F, NameIDEEO5CertifyingOfficial = F, NameIDW2CContactPerson = F, PERAEmployerNumber = F, TRACountyGroupNumber = F, TRADistrictUnitNumber = F, UnemploymentInsuranceAccountNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployerID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "Employer", objectId = EmployerID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Employer
	#'
	#' This function deletes an Employer
	#' @param EmployerID The ID of the Employer to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The EmployerID of the deleted Employer.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployer <- function(EmployerID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "Employer", objectId = EmployerID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Employer
	#'
	#' This function creates an Employer
	#' @param fieldNames The field values to give the created Employer. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created Employer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployer <- function(ACATransmitterControlCode = NULL, DistrictID = NULL, EEO4ControlNumber = NULL, EEO4JurisdictionName = NULL, EEO5NameOfCertifyingOfficial = NULL, EEO5OfficeOfSchoolNumber = NULL, EEO5SchoolDistrictName = NULL, EEOCTitleOfCertifyingOfficial = NULL, FederalEEO4FunctionID = NULL, NameID = NULL, NameIDACAContactPerson = NULL, NameIDEEO5CertifyingOfficial = NULL, NameIDW2CContactPerson = NULL, PERAEmployerNumber = NULL, TRACountyGroupNumber = NULL, TRADistrictUnitNumber = NULL, UnemploymentInsuranceAccountNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "Employer", body = list(DataObject = body), searchFields = append("EmployerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Employer
	#'
	#' This function modifies an Employer
	#' @param fieldNames The field values to give the modified Employer. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified Employer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployer <- function(EmployerID, ACATransmitterControlCode = NULL, DistrictID = NULL, EEO4ControlNumber = NULL, EEO4JurisdictionName = NULL, EEO5NameOfCertifyingOfficial = NULL, EEO5OfficeOfSchoolNumber = NULL, EEO5SchoolDistrictName = NULL, EEOCTitleOfCertifyingOfficial = NULL, FederalEEO4FunctionID = NULL, NameID = NULL, NameIDACAContactPerson = NULL, NameIDEEO5CertifyingOfficial = NULL, NameIDW2CContactPerson = NULL, PERAEmployerNumber = NULL, TRACountyGroupNumber = NULL, TRADistrictUnitNumber = NULL, UnemploymentInsuranceAccountNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "Employer", objectId = EmployerID, body = list(DataObject = body), searchFields = append("EmployerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NameVehicles
	#'
	#' This function returns a dataframe or json object of NameVehicles
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameVehicles. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameVehicles.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameVehicle') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of NameVehicles
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNameVehicles <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, NameID = F, NameVehicleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, VehicleID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "NameVehicle", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NameVehicle
	#'
	#' This function returns a dataframe or json object of a NameVehicle
	#' @param NameVehicleID The ID of the NameVehicle to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameVehicle. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameVehicle.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameVehicle') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of NameVehicle
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNameVehicle <- function(NameVehicleID, CreatedTime = F, ModifiedTime = F, NameID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, VehicleID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NameVehicleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "NameVehicle", objectId = NameVehicleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NameVehicle
	#'
	#' This function deletes a NameVehicle
	#' @param NameVehicleID The ID of the NameVehicle to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The NameVehicleID of the deleted NameVehicle.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNameVehicle <- function(NameVehicleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "NameVehicle", objectId = NameVehicleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NameVehicle
	#'
	#' This function creates a NameVehicle
	#' @param fieldNames The field values to give the created NameVehicle. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created NameVehicle
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNameVehicle <- function(NameID = NULL, VehicleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "NameVehicle", body = list(DataObject = body), searchFields = append("NameVehicleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NameVehicle
	#'
	#' This function modifies a NameVehicle
	#' @param fieldNames The field values to give the modified NameVehicle. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified NameVehicle
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNameVehicle <- function(NameVehicleID, NameID = NULL, VehicleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "NameVehicle", objectId = NameVehicleID, body = list(DataObject = body), searchFields = append("NameVehicleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempNameErrors
	#'
	#' This function returns a dataframe or json object of TempNameErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNameErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNameErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNameError') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of TempNameErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempNameErrors <- function(searchConditionsList = NULL, CreatedTime = F, FirstName = F, LastName = F, ModifiedTime = F, NameID = F, Note = F, TempNameErrorID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "TempNameError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempNameError
	#'
	#' This function returns a dataframe or json object of a TempNameError
	#' @param TempNameErrorID The ID of the TempNameError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNameError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNameError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNameError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of TempNameError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempNameError <- function(TempNameErrorID, CreatedTime = F, FirstName = F, LastName = F, ModifiedTime = F, NameID = F, Note = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempNameErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "TempNameError", objectId = TempNameErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempNameError
	#'
	#' This function deletes a TempNameError
	#' @param TempNameErrorID The ID of the TempNameError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The TempNameErrorID of the deleted TempNameError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempNameError <- function(TempNameErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "TempNameError", objectId = TempNameErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempNameError
	#'
	#' This function creates a TempNameError
	#' @param fieldNames The field values to give the created TempNameError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created TempNameError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempNameError <- function(FirstName = NULL, LastName = NULL, NameID = NULL, Note = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "TempNameError", body = list(DataObject = body), searchFields = append("TempNameErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempNameError
	#'
	#' This function modifies a TempNameError
	#' @param fieldNames The field values to give the modified TempNameError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified TempNameError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempNameError <- function(TempNameErrorID, FirstName = NULL, LastName = NULL, NameID = NULL, Note = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "TempNameError", objectId = TempNameErrorID, body = list(DataObject = body), searchFields = append("TempNameErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Vehicles
	#'
	#' This function returns a dataframe or json object of Vehicles
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Vehicles. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Vehicles.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Vehicle') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of Vehicles
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listVehicles <- function(searchConditionsList = NULL, Color = F, CreatedTime = F, LicensePlateNumber = F, MakeModel = F, ModifiedTime = F, PermitDate = F, PermitNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, VehicleID = F, VIN = F, Year = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "Vehicle", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Vehicle
	#'
	#' This function returns a dataframe or json object of a Vehicle
	#' @param VehicleID The ID of the Vehicle to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Vehicle. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Vehicle.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Vehicle') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of Vehicle
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getVehicle <- function(VehicleID, Color = F, CreatedTime = F, LicensePlateNumber = F, MakeModel = F, ModifiedTime = F, PermitDate = F, PermitNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, VIN = F, Year = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "VehicleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "Vehicle", objectId = VehicleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Vehicle
	#'
	#' This function deletes a Vehicle
	#' @param VehicleID The ID of the Vehicle to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The VehicleID of the deleted Vehicle.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteVehicle <- function(VehicleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "Vehicle", objectId = VehicleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Vehicle
	#'
	#' This function creates a Vehicle
	#' @param fieldNames The field values to give the created Vehicle. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created Vehicle
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createVehicle <- function(Color = NULL, LicensePlateNumber = NULL, MakeModel = NULL, PermitDate = NULL, PermitNumber = NULL, VIN = NULL, Year = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "Vehicle", body = list(DataObject = body), searchFields = append("VehicleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Vehicle
	#'
	#' This function modifies a Vehicle
	#' @param fieldNames The field values to give the modified Vehicle. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified Vehicle
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyVehicle <- function(VehicleID, Color = NULL, LicensePlateNumber = NULL, MakeModel = NULL, PermitDate = NULL, PermitNumber = NULL, VIN = NULL, Year = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "Vehicle", objectId = VehicleID, body = list(DataObject = body), searchFields = append("VehicleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempACHAccounts
	#'
	#' This function returns a dataframe or json object of TempACHAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempACHAccounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempACHAccounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempACHAccount') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of TempACHAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempACHAccounts <- function(searchConditionsList = NULL, AccountType = F, ACHAccountID = F, ACHAccountNumber = F, CreatedTime = F, IsEmployeeNetPayrollACH = F, IsVendorAccountsPayableACH = F, ModifiedTime = F, Name = F, RoutingNumber = F, TempACHAccountID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UseTaxAddenda = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "TempACHAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempACHAccount
	#'
	#' This function returns a dataframe or json object of a TempACHAccount
	#' @param TempACHAccountID The ID of the TempACHAccount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempACHAccount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempACHAccount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempACHAccount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of TempACHAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempACHAccount <- function(TempACHAccountID, AccountType = F, ACHAccountID = F, ACHAccountNumber = F, CreatedTime = F, IsEmployeeNetPayrollACH = F, IsVendorAccountsPayableACH = F, ModifiedTime = F, Name = F, RoutingNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UseTaxAddenda = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempACHAccountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "TempACHAccount", objectId = TempACHAccountID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempACHAccount
	#'
	#' This function deletes a TempACHAccount
	#' @param TempACHAccountID The ID of the TempACHAccount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The TempACHAccountID of the deleted TempACHAccount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempACHAccount <- function(TempACHAccountID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "TempACHAccount", objectId = TempACHAccountID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempACHAccount
	#'
	#' This function creates a TempACHAccount
	#' @param fieldNames The field values to give the created TempACHAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created TempACHAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempACHAccount <- function(AccountType = NULL, ACHAccountID = NULL, ACHAccountNumber = NULL, IsEmployeeNetPayrollACH = NULL, IsVendorAccountsPayableACH = NULL, Name = NULL, RoutingNumber = NULL, UseTaxAddenda = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "TempACHAccount", body = list(DataObject = body), searchFields = append("TempACHAccountID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempACHAccount
	#'
	#' This function modifies a TempACHAccount
	#' @param fieldNames The field values to give the modified TempACHAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified TempACHAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempACHAccount <- function(TempACHAccountID, AccountType = NULL, ACHAccountID = NULL, ACHAccountNumber = NULL, IsEmployeeNetPayrollACH = NULL, IsVendorAccountsPayableACH = NULL, Name = NULL, RoutingNumber = NULL, UseTaxAddenda = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "TempACHAccount", objectId = TempACHAccountID, body = list(DataObject = body), searchFields = append("TempACHAccountID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ACHAccounts
	#'
	#' This function returns a dataframe or json object of ACHAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ACHAccounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ACHAccounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ACHAccount') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of ACHAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listACHAccounts <- function(searchConditionsList = NULL, AccountType = F, ACHAccountID = F, ACHAccountNumber = F, Class = F, CompanyEntryDescription = F, CreatedTime = F, IsActive = F, IsEmployeeChildSupportACH = F, IsEmployeeNetPayrollACH = F, IsStateDisbursementUnit = F, IsVendorAccountsPayableACH = F, ModifiedTime = F, NameID = F, PrenoteDate = F, ReceivingCompany = F, RoutingNumber = F, StateIDSDU = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "ACHAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ACHAccount
	#'
	#' This function returns a dataframe or json object of an ACHAccount
	#' @param ACHAccountID The ID of the ACHAccount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ACHAccount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ACHAccount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ACHAccount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of ACHAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getACHAccount <- function(ACHAccountID, AccountType = F, ACHAccountNumber = F, Class = F, CompanyEntryDescription = F, CreatedTime = F, IsActive = F, IsEmployeeChildSupportACH = F, IsEmployeeNetPayrollACH = F, IsStateDisbursementUnit = F, IsVendorAccountsPayableACH = F, ModifiedTime = F, NameID = F, PrenoteDate = F, ReceivingCompany = F, RoutingNumber = F, StateIDSDU = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ACHAccountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "ACHAccount", objectId = ACHAccountID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ACHAccount
	#'
	#' This function deletes an ACHAccount
	#' @param ACHAccountID The ID of the ACHAccount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The ACHAccountID of the deleted ACHAccount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteACHAccount <- function(ACHAccountID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "ACHAccount", objectId = ACHAccountID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ACHAccount
	#'
	#' This function creates an ACHAccount
	#' @param fieldNames The field values to give the created ACHAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created ACHAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createACHAccount <- function(AccountType = NULL, ACHAccountNumber = NULL, Class = NULL, CompanyEntryDescription = NULL, IsActive = NULL, IsStateDisbursementUnit = NULL, NameID = NULL, PrenoteDate = NULL, ReceivingCompany = NULL, RoutingNumber = NULL, StateIDSDU = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "ACHAccount", body = list(DataObject = body), searchFields = append("ACHAccountID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ACHAccount
	#'
	#' This function modifies an ACHAccount
	#' @param fieldNames The field values to give the modified ACHAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified ACHAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyACHAccount <- function(ACHAccountID, AccountType = NULL, ACHAccountNumber = NULL, Class = NULL, CompanyEntryDescription = NULL, IsActive = NULL, IsStateDisbursementUnit = NULL, NameID = NULL, PrenoteDate = NULL, ReceivingCompany = NULL, RoutingNumber = NULL, StateIDSDU = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "ACHAccount", objectId = ACHAccountID, body = list(DataObject = body), searchFields = append("ACHAccountID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmergencyContacts
	#'
	#' This function returns a dataframe or json object of EmergencyContacts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmergencyContacts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmergencyContacts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmergencyContact') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of EmergencyContacts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmergencyContacts <- function(searchConditionsList = NULL, AllowPickUp = F, Comment = F, CreatedTime = F, EmergencyContactID = F, ModifiedTime = F, NameID = F, NameIDEmergencyContact = F, Rank = F, RelationshipID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "EmergencyContact", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmergencyContact
	#'
	#' This function returns a dataframe or json object of an EmergencyContact
	#' @param EmergencyContactID The ID of the EmergencyContact to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmergencyContact. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmergencyContact.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmergencyContact') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of EmergencyContact
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmergencyContact <- function(EmergencyContactID, AllowPickUp = F, Comment = F, CreatedTime = F, ModifiedTime = F, NameID = F, NameIDEmergencyContact = F, Rank = F, RelationshipID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmergencyContactID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "EmergencyContact", objectId = EmergencyContactID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmergencyContact
	#'
	#' This function deletes an EmergencyContact
	#' @param EmergencyContactID The ID of the EmergencyContact to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The EmergencyContactID of the deleted EmergencyContact.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmergencyContact <- function(EmergencyContactID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "EmergencyContact", objectId = EmergencyContactID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmergencyContact
	#'
	#' This function creates an EmergencyContact
	#' @param fieldNames The field values to give the created EmergencyContact. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created EmergencyContact
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmergencyContact <- function(AllowPickUp = NULL, Comment = NULL, NameID = NULL, NameIDEmergencyContact = NULL, Rank = NULL, RelationshipID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "EmergencyContact", body = list(DataObject = body), searchFields = append("EmergencyContactID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmergencyContact
	#'
	#' This function modifies an EmergencyContact
	#' @param fieldNames The field values to give the modified EmergencyContact. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified EmergencyContact
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmergencyContact <- function(EmergencyContactID, AllowPickUp = NULL, Comment = NULL, NameID = NULL, NameIDEmergencyContact = NULL, Rank = NULL, RelationshipID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "EmergencyContact", objectId = EmergencyContactID, body = list(DataObject = body), searchFields = append("EmergencyContactID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NameAddresses
	#'
	#' This function returns a dataframe or json object of NameAddresses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameAddresses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameAddresses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameAddress') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of NameAddresses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNameAddresses <- function(searchConditionsList = NULL, AddressID = F, CreatedTime = F, GetAddressType = F, HideFromDirectoryMA = F, Is1099 = F, IsBusDropoff = F, IsBusPickup = F, IsMailing = F, IsOrderFrom = F, IsPhysical = F, IsPrimaryGuardian = F, IsRemitTo = F, IsW2 = F, ModifiedTime = F, NameAddressID = F, NameID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "NameAddress", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NameAddress
	#'
	#' This function returns a dataframe or json object of a NameAddress
	#' @param NameAddressID The ID of the NameAddress to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameAddress. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameAddress.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameAddress') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of NameAddress
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNameAddress <- function(NameAddressID, AddressID = F, CreatedTime = F, GetAddressType = F, HideFromDirectoryMA = F, Is1099 = F, IsBusDropoff = F, IsBusPickup = F, IsMailing = F, IsOrderFrom = F, IsPhysical = F, IsPrimaryGuardian = F, IsRemitTo = F, IsW2 = F, ModifiedTime = F, NameID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NameAddressID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "NameAddress", objectId = NameAddressID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NameAddress
	#'
	#' This function deletes a NameAddress
	#' @param NameAddressID The ID of the NameAddress to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The NameAddressID of the deleted NameAddress.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNameAddress <- function(NameAddressID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "NameAddress", objectId = NameAddressID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NameAddress
	#'
	#' This function creates a NameAddress
	#' @param fieldNames The field values to give the created NameAddress. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created NameAddress
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNameAddress <- function(AddressID = NULL, HideFromDirectoryMA = NULL, Is1099 = NULL, IsBusDropoff = NULL, IsBusPickup = NULL, IsMailing = NULL, IsOrderFrom = NULL, IsPhysical = NULL, IsPrimaryGuardian = NULL, IsRemitTo = NULL, IsW2 = NULL, NameID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "NameAddress", body = list(DataObject = body), searchFields = append("NameAddressID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NameAddress
	#'
	#' This function modifies a NameAddress
	#' @param fieldNames The field values to give the modified NameAddress. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified NameAddress
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNameAddress <- function(NameAddressID, AddressID = NULL, HideFromDirectoryMA = NULL, Is1099 = NULL, IsBusDropoff = NULL, IsBusPickup = NULL, IsMailing = NULL, IsOrderFrom = NULL, IsPhysical = NULL, IsPrimaryGuardian = NULL, IsRemitTo = NULL, IsW2 = NULL, NameID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "NameAddress", objectId = NameAddressID, body = list(DataObject = body), searchFields = append("NameAddressID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DemographicsRelationships
	#'
	#' This function returns a dataframe or json object of DemographicsRelationships
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DemographicsRelationships. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DemographicsRelationships.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DemographicsRelationship') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of DemographicsRelationships
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDemographicsRelationships <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EdFiRelationDescriptorID = F, EdFiRelationTypeID = F, GrandPeopleMAID = F, ModifiedTime = F, RelationshipID = F, RelationshipType = F, ShortenedDescription = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "Relationship", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DemographicsRelationship
	#'
	#' This function returns a dataframe or json object of a DemographicsRelationship
	#' @param DemographicsRelationshipID The ID of the DemographicsRelationship to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DemographicsRelationship. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DemographicsRelationship.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DemographicsRelationship') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of DemographicsRelationship
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDemographicsRelationship <- function(DemographicsRelationshipID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EdFiRelationDescriptorID = F, EdFiRelationTypeID = F, GrandPeopleMAID = F, ModifiedTime = F, RelationshipID = F, RelationshipType = F, ShortenedDescription = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DemographicsRelationshipID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "Relationship", objectId = DemographicsRelationshipID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DemographicsRelationship
	#'
	#' This function deletes a DemographicsRelationship
	#' @param DemographicsRelationshipID The ID of the DemographicsRelationship to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The DemographicsRelationshipID of the deleted DemographicsRelationship.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDemographicsRelationship <- function(DemographicsRelationshipID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "Relationship", objectId = DemographicsRelationshipID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DemographicsRelationship
	#'
	#' This function creates a DemographicsRelationship
	#' @param fieldNames The field values to give the created DemographicsRelationship. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created DemographicsRelationship
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDemographicsRelationship <- function(Code = NULL, Description = NULL, EdFiRelationDescriptorID = NULL, EdFiRelationTypeID = NULL, GrandPeopleMAID = NULL, RelationshipType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "Relationship", body = list(DataObject = body), searchFields = append("RelationshipID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DemographicsRelationship
	#'
	#' This function modifies a DemographicsRelationship
	#' @param fieldNames The field values to give the modified DemographicsRelationship. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified DemographicsRelationship
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDemographicsRelationship <- function(RelationshipID, Code = NULL, Description = NULL, EdFiRelationDescriptorID = NULL, EdFiRelationTypeID = NULL, GrandPeopleMAID = NULL, RelationshipType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "Relationship", objectId = RelationshipID, body = list(DataObject = body), searchFields = append("RelationshipID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Institutions
	#'
	#' This function returns a dataframe or json object of Institutions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Institutions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Institutions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Institution') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of Institutions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInstitutions <- function(searchConditionsList = NULL, CEEBCode = F, CreatedTime = F, InstitutionDefaultID = F, InstitutionID = F, InstitutionMNID = F, MCCCCollegeCode = F, ModifiedTime = F, NameID = F, StateID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "Institution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Institution
	#'
	#' This function returns a dataframe or json object of an Institution
	#' @param InstitutionID The ID of the Institution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Institution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Institution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Institution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of Institution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInstitution <- function(InstitutionID, CEEBCode = F, CreatedTime = F, InstitutionDefaultID = F, InstitutionMNID = F, MCCCCollegeCode = F, ModifiedTime = F, NameID = F, StateID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InstitutionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "Institution", objectId = InstitutionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Institution
	#'
	#' This function deletes an Institution
	#' @param InstitutionID The ID of the Institution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The InstitutionID of the deleted Institution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInstitution <- function(InstitutionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "Institution", objectId = InstitutionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Institution
	#'
	#' This function creates an Institution
	#' @param fieldNames The field values to give the created Institution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created Institution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInstitution <- function(CEEBCode = NULL, InstitutionDefaultID = NULL, MCCCCollegeCode = NULL, NameID = NULL, StateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "Institution", body = list(DataObject = body), searchFields = append("InstitutionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Institution
	#'
	#' This function modifies an Institution
	#' @param fieldNames The field values to give the modified Institution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified Institution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInstitution <- function(InstitutionID, CEEBCode = NULL, InstitutionDefaultID = NULL, MCCCCollegeCode = NULL, NameID = NULL, StateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "Institution", objectId = InstitutionID, body = list(DataObject = body), searchFields = append("InstitutionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Addresses
	#'
	#' This function returns a dataframe or json object of Addresses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Addresses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Addresses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Address') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of Addresses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAddresses <- function(searchConditionsList = NULL, AddressID = F, AddressLine2 = F, AddressRangeNumericStreetNumber = F, AddressRangeNumericStreetNumberIsOdd = F, AddressSecondaryUnitID = F, AddressType = F, CountyID = F, CreatedTime = F, FormattedFullAddress = F, FreeformAddress = F, FullAddress = F, GeoID = F, InternationalAddress1 = F, InternationalAddress2 = F, InternationalAddress3 = F, InternationalAddress4 = F, IsLoadedLatitudeLongitude = F, Latitude = F, LatitudeLongitudeConfidence = F, Longitude = F, ModifiedTime = F, POBox = F, PrintableHTMLAddress = F, SecondaryUnitNumber = F, StreetID = F, StreetNumber = F, StreetNumberAndName = F, StreetNumberAndNameIncludeSecondaryUnit = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ZipCodeAddOn = F, ZipCodeWithAddon = F, ZipCodeWithAddonNoHyphen = F, ZipID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "Address", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Address
	#'
	#' This function returns a dataframe or json object of an Address
	#' @param AddressID The ID of the Address to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Address. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Address.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Address') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of Address
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAddress <- function(AddressID, AddressLine2 = F, AddressRangeNumericStreetNumber = F, AddressRangeNumericStreetNumberIsOdd = F, AddressSecondaryUnitID = F, AddressType = F, CountyID = F, CreatedTime = F, FormattedFullAddress = F, FreeformAddress = F, FullAddress = F, GeoID = F, InternationalAddress1 = F, InternationalAddress2 = F, InternationalAddress3 = F, InternationalAddress4 = F, IsLoadedLatitudeLongitude = F, Latitude = F, LatitudeLongitudeConfidence = F, Longitude = F, ModifiedTime = F, POBox = F, PrintableHTMLAddress = F, SecondaryUnitNumber = F, StreetID = F, StreetNumber = F, StreetNumberAndName = F, StreetNumberAndNameIncludeSecondaryUnit = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ZipCodeAddOn = F, ZipCodeWithAddon = F, ZipCodeWithAddonNoHyphen = F, ZipID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AddressID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "Address", objectId = AddressID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Address
	#'
	#' This function deletes an Address
	#' @param AddressID The ID of the Address to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The AddressID of the deleted Address.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAddress <- function(AddressID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "Address", objectId = AddressID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Address
	#'
	#' This function creates an Address
	#' @param fieldNames The field values to give the created Address. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created Address
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAddress <- function(AddressLine2 = NULL, AddressRangeNumericStreetNumber = NULL, AddressSecondaryUnitID = NULL, AddressType = NULL, CountyID = NULL, FreeformAddress = NULL, FullAddress = NULL, GeoID = NULL, InternationalAddress1 = NULL, InternationalAddress2 = NULL, InternationalAddress3 = NULL, InternationalAddress4 = NULL, IsLoadedLatitudeLongitude = NULL, Latitude = NULL, LatitudeLongitudeConfidence = NULL, Longitude = NULL, POBox = NULL, SecondaryUnitNumber = NULL, StreetID = NULL, StreetNumber = NULL, ZipCodeAddOn = NULL, ZipID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "Address", body = list(DataObject = body), searchFields = append("AddressID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Address
	#'
	#' This function modifies an Address
	#' @param fieldNames The field values to give the modified Address. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified Address
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAddress <- function(AddressID, AddressLine2 = NULL, AddressRangeNumericStreetNumber = NULL, AddressSecondaryUnitID = NULL, AddressType = NULL, CountyID = NULL, FreeformAddress = NULL, FullAddress = NULL, GeoID = NULL, InternationalAddress1 = NULL, InternationalAddress2 = NULL, InternationalAddress3 = NULL, InternationalAddress4 = NULL, IsLoadedLatitudeLongitude = NULL, Latitude = NULL, LatitudeLongitudeConfidence = NULL, Longitude = NULL, POBox = NULL, SecondaryUnitNumber = NULL, StreetID = NULL, StreetNumber = NULL, ZipCodeAddOn = NULL, ZipID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "Address", objectId = AddressID, body = list(DataObject = body), searchFields = append("AddressID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AddressSecondaryUnits
	#'
	#' This function returns a dataframe or json object of AddressSecondaryUnits
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AddressSecondaryUnits. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AddressSecondaryUnits.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AddressSecondaryUnit') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of AddressSecondaryUnits
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAddressSecondaryUnits <- function(searchConditionsList = NULL, AddressSecondaryUnitID = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, RequiresNumber = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "AddressSecondaryUnit", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AddressSecondaryUnit
	#'
	#' This function returns a dataframe or json object of an AddressSecondaryUnit
	#' @param AddressSecondaryUnitID The ID of the AddressSecondaryUnit to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AddressSecondaryUnit. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AddressSecondaryUnit.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AddressSecondaryUnit') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of AddressSecondaryUnit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAddressSecondaryUnit <- function(AddressSecondaryUnitID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, RequiresNumber = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AddressSecondaryUnitID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "AddressSecondaryUnit", objectId = AddressSecondaryUnitID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AddressSecondaryUnit
	#'
	#' This function deletes an AddressSecondaryUnit
	#' @param AddressSecondaryUnitID The ID of the AddressSecondaryUnit to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The AddressSecondaryUnitID of the deleted AddressSecondaryUnit.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAddressSecondaryUnit <- function(AddressSecondaryUnitID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "AddressSecondaryUnit", objectId = AddressSecondaryUnitID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AddressSecondaryUnit
	#'
	#' This function creates an AddressSecondaryUnit
	#' @param fieldNames The field values to give the created AddressSecondaryUnit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created AddressSecondaryUnit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAddressSecondaryUnit <- function(Code = NULL, Description = NULL, RequiresNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "AddressSecondaryUnit", body = list(DataObject = body), searchFields = append("AddressSecondaryUnitID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AddressSecondaryUnit
	#'
	#' This function modifies an AddressSecondaryUnit
	#' @param fieldNames The field values to give the modified AddressSecondaryUnit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified AddressSecondaryUnit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAddressSecondaryUnit <- function(AddressSecondaryUnitID, Code = NULL, Description = NULL, RequiresNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "AddressSecondaryUnit", objectId = AddressSecondaryUnitID, body = list(DataObject = body), searchFields = append("AddressSecondaryUnitID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Directionals
	#'
	#' This function returns a dataframe or json object of Directionals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Directionals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Directionals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Directional') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of Directionals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDirectionals <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DirectionalID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "Directional", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Directional
	#'
	#' This function returns a dataframe or json object of a Directional
	#' @param DirectionalID The ID of the Directional to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Directional. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Directional.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Directional') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of Directional
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDirectional <- function(DirectionalID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DirectionalID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "Directional", objectId = DirectionalID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Directional
	#'
	#' This function deletes a Directional
	#' @param DirectionalID The ID of the Directional to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The DirectionalID of the deleted Directional.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDirectional <- function(DirectionalID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "Directional", objectId = DirectionalID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Directional
	#'
	#' This function creates a Directional
	#' @param fieldNames The field values to give the created Directional. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created Directional
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDirectional <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "Directional", body = list(DataObject = body), searchFields = append("DirectionalID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Directional
	#'
	#' This function modifies a Directional
	#' @param fieldNames The field values to give the modified Directional. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified Directional
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDirectional <- function(DirectionalID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "Directional", objectId = DirectionalID, body = list(DataObject = body), searchFields = append("DirectionalID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Languages
	#'
	#' This function returns a dataframe or json object of Languages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Languages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Languages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Language') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of Languages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLanguages <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, CurrentStateLanguage = F, CurrentStateLanguageCode = F, CurrentStateLanguageID = F, Description = F, EdFiLanguageDescriptorID = F, LanguageID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "Language", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Language
	#'
	#' This function returns a dataframe or json object of a Language
	#' @param LanguageID The ID of the Language to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Language. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Language.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Language') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of Language
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLanguage <- function(LanguageID, Code = F, CodeDescription = F, CreatedTime = F, CurrentStateLanguage = F, CurrentStateLanguageCode = F, CurrentStateLanguageID = F, Description = F, EdFiLanguageDescriptorID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LanguageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "Language", objectId = LanguageID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Language
	#'
	#' This function deletes a Language
	#' @param LanguageID The ID of the Language to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The LanguageID of the deleted Language.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLanguage <- function(LanguageID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "Language", objectId = LanguageID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Language
	#'
	#' This function creates a Language
	#' @param fieldNames The field values to give the created Language. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created Language
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLanguage <- function(Code = NULL, Description = NULL, EdFiLanguageDescriptorID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "Language", body = list(DataObject = body), searchFields = append("LanguageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Language
	#'
	#' This function modifies a Language
	#' @param fieldNames The field values to give the modified Language. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified Language
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLanguage <- function(LanguageID, Code = NULL, Description = NULL, EdFiLanguageDescriptorID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "Language", objectId = LanguageID, body = list(DataObject = body), searchFields = append("LanguageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Streets
	#'
	#' This function returns a dataframe or json object of Streets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Streets. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Streets.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Street') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of Streets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStreets <- function(searchConditionsList = NULL, CreatedTime = F, DirectionalID = F, FormattedStreet = F, ModifiedTime = F, Name = F, StreetID = F, StreetNameWithDirectionCode = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ZipID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "Street", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Street
	#'
	#' This function returns a dataframe or json object of a Street
	#' @param StreetID The ID of the Street to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Street. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Street.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Street') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of Street
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStreet <- function(StreetID, CreatedTime = F, DirectionalID = F, FormattedStreet = F, ModifiedTime = F, Name = F, StreetNameWithDirectionCode = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ZipID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StreetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "Street", objectId = StreetID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Street
	#'
	#' This function deletes a Street
	#' @param StreetID The ID of the Street to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The StreetID of the deleted Street.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStreet <- function(StreetID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "Street", objectId = StreetID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Street
	#'
	#' This function creates a Street
	#' @param fieldNames The field values to give the created Street. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created Street
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStreet <- function(DirectionalID = NULL, Name = NULL, ZipID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "Street", body = list(DataObject = body), searchFields = append("StreetID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Street
	#'
	#' This function modifies a Street
	#' @param fieldNames The field values to give the modified Street. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified Street
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStreet <- function(StreetID, DirectionalID = NULL, Name = NULL, ZipID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "Street", objectId = StreetID, body = list(DataObject = body), searchFields = append("StreetID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Zips
	#'
	#' This function returns a dataframe or json object of Zips
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Zips. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Zips.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Zip') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of Zips
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listZips <- function(searchConditionsList = NULL, City = F, CityState = F, CityStateCode = F, CityStateZip = F, CityZipCode = F, CountryCode = F, CreatedTime = F, EdFiCountryID = F, FreeFormCountry = F, FreeFormState = F, IsPreferredByUSPS = F, ModifiedTime = F, StateID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ZipCode = F, ZipID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "Zip", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Zip
	#'
	#' This function returns a dataframe or json object of a Zip
	#' @param ZipID The ID of the Zip to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Zip. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Zip.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Zip') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of Zip
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getZip <- function(ZipID, City = F, CityState = F, CityStateCode = F, CityStateZip = F, CityZipCode = F, CountryCode = F, CreatedTime = F, EdFiCountryID = F, FreeFormCountry = F, FreeFormState = F, IsPreferredByUSPS = F, ModifiedTime = F, StateID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ZipCode = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ZipID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "Zip", objectId = ZipID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Zip
	#'
	#' This function deletes a Zip
	#' @param ZipID The ID of the Zip to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The ZipID of the deleted Zip.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteZip <- function(ZipID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "Zip", objectId = ZipID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Zip
	#'
	#' This function creates a Zip
	#' @param fieldNames The field values to give the created Zip. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created Zip
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createZip <- function(City = NULL, CountryCode = NULL, EdFiCountryID = NULL, FreeFormCountry = NULL, FreeFormState = NULL, IsPreferredByUSPS = NULL, StateID = NULL, ZipCode = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "Zip", body = list(DataObject = body), searchFields = append("ZipID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Zip
	#'
	#' This function modifies a Zip
	#' @param fieldNames The field values to give the modified Zip. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified Zip
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyZip <- function(ZipID, City = NULL, CountryCode = NULL, EdFiCountryID = NULL, FreeFormCountry = NULL, FreeFormState = NULL, IsPreferredByUSPS = NULL, StateID = NULL, ZipCode = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "Zip", objectId = ZipID, body = list(DataObject = body), searchFields = append("ZipID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PhoneTypes
	#'
	#' This function returns a dataframe or json object of PhoneTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PhoneTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PhoneTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PhoneType') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of PhoneTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPhoneTypes <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EdFiTelephoneNumberTypeDescriptorID = F, ModifiedTime = F, PhoneTypeID = F, PreventFamilyStudentAccessUpdates = F, ReadonlyFamilyStudentAccess = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "PhoneType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PhoneType
	#'
	#' This function returns a dataframe or json object of a PhoneType
	#' @param PhoneTypeID The ID of the PhoneType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PhoneType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PhoneType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PhoneType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of PhoneType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPhoneType <- function(PhoneTypeID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EdFiTelephoneNumberTypeDescriptorID = F, ModifiedTime = F, PreventFamilyStudentAccessUpdates = F, ReadonlyFamilyStudentAccess = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PhoneTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "PhoneType", objectId = PhoneTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PhoneType
	#'
	#' This function deletes a PhoneType
	#' @param PhoneTypeID The ID of the PhoneType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The PhoneTypeID of the deleted PhoneType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePhoneType <- function(PhoneTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "PhoneType", objectId = PhoneTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PhoneType
	#'
	#' This function creates a PhoneType
	#' @param fieldNames The field values to give the created PhoneType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created PhoneType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPhoneType <- function(Code = NULL, Description = NULL, EdFiTelephoneNumberTypeDescriptorID = NULL, PreventFamilyStudentAccessUpdates = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "PhoneType", body = list(DataObject = body), searchFields = append("PhoneTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PhoneType
	#'
	#' This function modifies a PhoneType
	#' @param fieldNames The field values to give the modified PhoneType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified PhoneType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPhoneType <- function(PhoneTypeID, Code = NULL, Description = NULL, EdFiTelephoneNumberTypeDescriptorID = NULL, PreventFamilyStudentAccessUpdates = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "PhoneType", objectId = PhoneTypeID, body = list(DataObject = body), searchFields = append("PhoneTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NamePhones
	#'
	#' This function returns a dataframe or json object of NamePhones
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NamePhones. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NamePhones.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NamePhone') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of NamePhones
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNamePhones <- function(searchConditionsList = NULL, CreatedTime = F, Extension = F, FormattedPhoneNumber = F, FormattedPhoneNumberCodeEEL = F, FullPhoneNumber = F, IsConfidential = F, IsFax = F, IsInternational = F, IsPrimaryFax = F, ModifiedTime = F, NameID = F, NamePhoneID = F, Note = F, PhoneNumber = F, PhoneTypeID = F, Rank = F, UsedByHealthProfessional = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "NamePhone", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NamePhone
	#'
	#' This function returns a dataframe or json object of a NamePhone
	#' @param NamePhoneID The ID of the NamePhone to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NamePhone. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NamePhone.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NamePhone') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of NamePhone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNamePhone <- function(NamePhoneID, CreatedTime = F, Extension = F, FormattedPhoneNumber = F, FormattedPhoneNumberCodeEEL = F, FullPhoneNumber = F, IsConfidential = F, IsFax = F, IsInternational = F, IsPrimaryFax = F, ModifiedTime = F, NameID = F, Note = F, PhoneNumber = F, PhoneTypeID = F, Rank = F, UsedByHealthProfessional = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NamePhoneID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "NamePhone", objectId = NamePhoneID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NamePhone
	#'
	#' This function deletes a NamePhone
	#' @param NamePhoneID The ID of the NamePhone to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The NamePhoneID of the deleted NamePhone.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNamePhone <- function(NamePhoneID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "NamePhone", objectId = NamePhoneID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NamePhone
	#'
	#' This function creates a NamePhone
	#' @param fieldNames The field values to give the created NamePhone. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created NamePhone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNamePhone <- function(Extension = NULL, IsConfidential = NULL, IsFax = NULL, IsInternational = NULL, NameID = NULL, Note = NULL, PhoneNumber = NULL, PhoneTypeID = NULL, Rank = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "NamePhone", body = list(DataObject = body), searchFields = append("NamePhoneID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NamePhone
	#'
	#' This function modifies a NamePhone
	#' @param fieldNames The field values to give the modified NamePhone. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified NamePhone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNamePhone <- function(NamePhoneID, Extension = NULL, IsConfidential = NULL, IsFax = NULL, IsInternational = NULL, NameID = NULL, Note = NULL, PhoneNumber = NULL, PhoneTypeID = NULL, Rank = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "NamePhone", objectId = NamePhoneID, body = list(DataObject = body), searchFields = append("NamePhoneID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmailTypes
	#'
	#' This function returns a dataframe or json object of EmailTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmailTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmailTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmailType') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of EmailTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmailTypes <- function(searchConditionsList = NULL, ARConfigDistrictEmailTypeRank = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EdFiElectronicMailTypeDescriptorID = F, EmailTypeID = F, ModifiedTime = F, PreventFamilyStudentAccessUpdates = F, ReadonlyFamilyStudentAccess = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "EmailType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmailType
	#'
	#' This function returns a dataframe or json object of an EmailType
	#' @param EmailTypeID The ID of the EmailType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmailType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmailType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmailType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of EmailType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmailType <- function(EmailTypeID, ARConfigDistrictEmailTypeRank = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EdFiElectronicMailTypeDescriptorID = F, ModifiedTime = F, PreventFamilyStudentAccessUpdates = F, ReadonlyFamilyStudentAccess = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmailTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "EmailType", objectId = EmailTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmailType
	#'
	#' This function deletes an EmailType
	#' @param EmailTypeID The ID of the EmailType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The EmailTypeID of the deleted EmailType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmailType <- function(EmailTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "EmailType", objectId = EmailTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmailType
	#'
	#' This function creates an EmailType
	#' @param fieldNames The field values to give the created EmailType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created EmailType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmailType <- function(Code = NULL, Description = NULL, EdFiElectronicMailTypeDescriptorID = NULL, PreventFamilyStudentAccessUpdates = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "EmailType", body = list(DataObject = body), searchFields = append("EmailTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmailType
	#'
	#' This function modifies an EmailType
	#' @param fieldNames The field values to give the modified EmailType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified EmailType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmailType <- function(EmailTypeID, Code = NULL, Description = NULL, EdFiElectronicMailTypeDescriptorID = NULL, PreventFamilyStudentAccessUpdates = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "EmailType", objectId = EmailTypeID, body = list(DataObject = body), searchFields = append("EmailTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NameEmails
	#'
	#' This function returns a dataframe or json object of NameEmails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameEmails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameEmails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameEmail') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of NameEmails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNameEmails <- function(searchConditionsList = NULL, CreatedTime = F, EmailAddress = F, EmailTypeID = F, IsAccountsPayableDirectDepositNotificationEmail = F, ModifiedTime = F, NameEmailID = F, NameID = F, Note = F, Rank = F, UsedByHealthProfessional = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "NameEmail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NameEmail
	#'
	#' This function returns a dataframe or json object of a NameEmail
	#' @param NameEmailID The ID of the NameEmail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameEmail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameEmail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameEmail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of NameEmail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNameEmail <- function(NameEmailID, CreatedTime = F, EmailAddress = F, EmailTypeID = F, IsAccountsPayableDirectDepositNotificationEmail = F, ModifiedTime = F, NameID = F, Note = F, Rank = F, UsedByHealthProfessional = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NameEmailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "NameEmail", objectId = NameEmailID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NameEmail
	#'
	#' This function deletes a NameEmail
	#' @param NameEmailID The ID of the NameEmail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The NameEmailID of the deleted NameEmail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNameEmail <- function(NameEmailID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "NameEmail", objectId = NameEmailID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NameEmail
	#'
	#' This function creates a NameEmail
	#' @param fieldNames The field values to give the created NameEmail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created NameEmail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNameEmail <- function(EmailAddress = NULL, EmailTypeID = NULL, NameID = NULL, Note = NULL, Rank = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "NameEmail", body = list(DataObject = body), searchFields = append("NameEmailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NameEmail
	#'
	#' This function modifies a NameEmail
	#' @param fieldNames The field values to give the modified NameEmail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified NameEmail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNameEmail <- function(NameEmailID, EmailAddress = NULL, EmailTypeID = NULL, NameID = NULL, Note = NULL, Rank = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "NameEmail", objectId = NameEmailID, body = list(DataObject = body), searchFields = append("NameEmailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NameSuffixes
	#'
	#' This function returns a dataframe or json object of NameSuffixes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameSuffixes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameSuffixes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameSuffix') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of NameSuffixes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNameSuffixes <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, NameSuffixID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "NameSuffix", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NameSuffix
	#'
	#' This function returns a dataframe or json object of a NameSuffix
	#' @param NameSuffixID The ID of the NameSuffix to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameSuffix. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameSuffix.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameSuffix') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of NameSuffix
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNameSuffix <- function(NameSuffixID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NameSuffixID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "NameSuffix", objectId = NameSuffixID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NameSuffix
	#'
	#' This function deletes a NameSuffix
	#' @param NameSuffixID The ID of the NameSuffix to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The NameSuffixID of the deleted NameSuffix.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNameSuffix <- function(NameSuffixID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "NameSuffix", objectId = NameSuffixID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NameSuffix
	#'
	#' This function creates a NameSuffix
	#' @param fieldNames The field values to give the created NameSuffix. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created NameSuffix
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNameSuffix <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "NameSuffix", body = list(DataObject = body), searchFields = append("NameSuffixID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NameSuffix
	#'
	#' This function modifies a NameSuffix
	#' @param fieldNames The field values to give the modified NameSuffix. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified NameSuffix
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNameSuffix <- function(NameSuffixID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "NameSuffix", objectId = NameSuffixID, body = list(DataObject = body), searchFields = append("NameSuffixID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NameTitles
	#'
	#' This function returns a dataframe or json object of NameTitles
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameTitles. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameTitles.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameTitle') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of NameTitles
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNameTitles <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, NameTitleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "NameTitle", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NameTitle
	#'
	#' This function returns a dataframe or json object of a NameTitle
	#' @param NameTitleID The ID of the NameTitle to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameTitle. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameTitle.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameTitle') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of NameTitle
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNameTitle <- function(NameTitleID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NameTitleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "NameTitle", objectId = NameTitleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NameTitle
	#'
	#' This function deletes a NameTitle
	#' @param NameTitleID The ID of the NameTitle to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The NameTitleID of the deleted NameTitle.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNameTitle <- function(NameTitleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "NameTitle", objectId = NameTitleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NameTitle
	#'
	#' This function creates a NameTitle
	#' @param fieldNames The field values to give the created NameTitle. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created NameTitle
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNameTitle <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "NameTitle", body = list(DataObject = body), searchFields = append("NameTitleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NameTitle
	#'
	#' This function modifies a NameTitle
	#' @param fieldNames The field values to give the modified NameTitle. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified NameTitle
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNameTitle <- function(NameTitleID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "NameTitle", objectId = NameTitleID, body = list(DataObject = body), searchFields = append("NameTitleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Names
	#'
	#' This function returns a dataframe or json object of Names
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Names. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Names.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Name') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of Names
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNames <- function(searchConditionsList = NULL, Age = F, BirthDate = F, BirthMonth = F, BirthMonthDay = F, BirthYear = F, BypassStudentRaceValidation = F, CalculatedPrimaryFormattedPhoneNumber = F, ContactPerson = F, ConversionKey = F, CreatedTime = F, DriversLicenseNumber = F, EducationLevelIDHighestCompleted = F, Ethnicity = F, EthnicityAndRace = F, ExternalUniqueIdentifier = F, FamilyStudentAccessStaffNameToUse = F, FederalEIN = F, FirstInitial = F, FirstInitialLastName = F, FirstInitialLastNameLegal = F, FirstInitialLegal = F, FirstName = F, FirstNameLegal = F, FullNameFL = F, FullNameFMIL = F, FullNameFML = F, FullNameLegalFL = F, FullNameLegalFML = F, FullNameLegalLFM = F, FullNameLF = F, FullNameLFM = F, Gender = F, GetNameUse = F, GrandPeopleMAID = F, GrandPersonMAID = F, HasCompletedWellnessScreeningForSpecifiedDate = F, HasMailingOrPhysicalAddress = F, HasPassedWellnessScreeningForSpecifiedDate = F, Initials = F, InitialsLegal = F, IsAlaskan = F, IsAsian = F, IsBlack = F, IsBusiness = F, IsCurrentStudent = F, IsEmergencyContactName = F, IsEmployeeName = F, IsEmployeeNameForDistrict = F, IsFeeManagementCustomerName = F, IsFeeManagementPayorName = F, IsFoodServiceCustomerName = F, IsFoodServicePayorName = F, IsGuardianName = F, IsHawaiian = F, IsHealthProfessionalName = F, IsHispanic = F, IsInstitution = F, IsPayorName = F, IsPayorNameForDistrict = F, IsPlaceOfEmployment = F, IsSingleLegalName = F, IsSkyward = F, IsStaffName = F, IsStudentInDistrict = F, IsStudentName = F, IsUserName = F, IsVendorName = F, IsVendorNameForDistrict = F, IsWhite = F, LastInitial = F, LastInitialLegal = F, LastName = F, LastNameFirstInitial = F, LastNameLegal = F, MaritalStatus = F, MaskedSocialSecurityNumber = F, MediaIDPhoto = F, MiddleInitial = F, MiddleInitialLegal = F, MiddleName = F, MiddleNameLegal = F, ModifiedTime = F, NameAddressMailingID = F, NameID = F, NameIDPlaceOfEmployment = F, NameKey = F, NameNumber = F, NameSuffixID = F, NameSuffixIDLegal = F, NameTitleID = F, NameTitleIDLegal = F, OccupationID = F, Race = F, RaceEduphoriaCode = F, RaceVerifiedBy = F, RaceVerifiedDate = F, SkywardHash = F, SkywardID = F, SocialSecurityNumber = F, SpecifySeparateLegalName = F, TitledName = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "Name", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Name
	#'
	#' This function returns a dataframe or json object of a Name
	#' @param NameID The ID of the Name to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Name. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Name.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Name') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of Name
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getName <- function(NameID, Age = F, BirthDate = F, BirthMonth = F, BirthMonthDay = F, BirthYear = F, BypassStudentRaceValidation = F, CalculatedPrimaryFormattedPhoneNumber = F, ContactPerson = F, ConversionKey = F, CreatedTime = F, DriversLicenseNumber = F, EducationLevelIDHighestCompleted = F, Ethnicity = F, EthnicityAndRace = F, ExternalUniqueIdentifier = F, FamilyStudentAccessStaffNameToUse = F, FederalEIN = F, FirstInitial = F, FirstInitialLastName = F, FirstInitialLastNameLegal = F, FirstInitialLegal = F, FirstName = F, FirstNameLegal = F, FullNameFL = F, FullNameFMIL = F, FullNameFML = F, FullNameLegalFL = F, FullNameLegalFML = F, FullNameLegalLFM = F, FullNameLF = F, FullNameLFM = F, Gender = F, GetNameUse = F, GrandPeopleMAID = F, GrandPersonMAID = F, HasCompletedWellnessScreeningForSpecifiedDate = F, HasMailingOrPhysicalAddress = F, HasPassedWellnessScreeningForSpecifiedDate = F, Initials = F, InitialsLegal = F, IsAlaskan = F, IsAsian = F, IsBlack = F, IsBusiness = F, IsCurrentStudent = F, IsEmergencyContactName = F, IsEmployeeName = F, IsEmployeeNameForDistrict = F, IsFeeManagementCustomerName = F, IsFeeManagementPayorName = F, IsFoodServiceCustomerName = F, IsFoodServicePayorName = F, IsGuardianName = F, IsHawaiian = F, IsHealthProfessionalName = F, IsHispanic = F, IsInstitution = F, IsPayorName = F, IsPayorNameForDistrict = F, IsPlaceOfEmployment = F, IsSingleLegalName = F, IsSkyward = F, IsStaffName = F, IsStudentInDistrict = F, IsStudentName = F, IsUserName = F, IsVendorName = F, IsVendorNameForDistrict = F, IsWhite = F, LastInitial = F, LastInitialLegal = F, LastName = F, LastNameFirstInitial = F, LastNameLegal = F, MaritalStatus = F, MaskedSocialSecurityNumber = F, MediaIDPhoto = F, MiddleInitial = F, MiddleInitialLegal = F, MiddleName = F, MiddleNameLegal = F, ModifiedTime = F, NameAddressMailingID = F, NameIDPlaceOfEmployment = F, NameKey = F, NameNumber = F, NameSuffixID = F, NameSuffixIDLegal = F, NameTitleID = F, NameTitleIDLegal = F, OccupationID = F, Race = F, RaceEduphoriaCode = F, RaceVerifiedBy = F, RaceVerifiedDate = F, SkywardHash = F, SkywardID = F, SocialSecurityNumber = F, SpecifySeparateLegalName = F, TitledName = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NameID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "Name", objectId = NameID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Name
	#'
	#' This function deletes a Name
	#' @param NameID The ID of the Name to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The NameID of the deleted Name.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteName <- function(NameID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "Name", objectId = NameID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Name
	#'
	#' This function creates a Name
	#' @param fieldNames The field values to give the created Name. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created Name
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createName <- function(BirthDate = NULL, ContactPerson = NULL, ConversionKey = NULL, DriversLicenseNumber = NULL, EducationLevelIDHighestCompleted = NULL, ExternalUniqueIdentifier = NULL, FederalEIN = NULL, FirstName = NULL, FirstNameLegal = NULL, Gender = NULL, GrandPeopleMAID = NULL, GrandPersonMAID = NULL, IsAlaskan = NULL, IsAsian = NULL, IsBlack = NULL, IsBusiness = NULL, IsEmergencyContactName = NULL, IsEmployeeName = NULL, IsFeeManagementCustomerName = NULL, IsFeeManagementPayorName = NULL, IsFoodServiceCustomerName = NULL, IsFoodServicePayorName = NULL, IsGuardianName = NULL, IsHawaiian = NULL, IsHealthProfessionalName = NULL, IsHispanic = NULL, IsInstitution = NULL, IsPayorName = NULL, IsPlaceOfEmployment = NULL, IsSingleLegalName = NULL, IsStaffName = NULL, IsStudentName = NULL, IsUserName = NULL, IsVendorName = NULL, IsWhite = NULL, LastName = NULL, LastNameLegal = NULL, MaritalStatus = NULL, MediaIDPhoto = NULL, MiddleName = NULL, MiddleNameLegal = NULL, NameIDPlaceOfEmployment = NULL, NameKey = NULL, NameSuffixID = NULL, NameSuffixIDLegal = NULL, NameTitleID = NULL, NameTitleIDLegal = NULL, OccupationID = NULL, RaceVerifiedBy = NULL, RaceVerifiedDate = NULL, SocialSecurityNumber = NULL, SpecifySeparateLegalName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "Name", body = list(DataObject = body), searchFields = append("NameID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Name
	#'
	#' This function modifies a Name
	#' @param fieldNames The field values to give the modified Name. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified Name
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyName <- function(NameID, BirthDate = NULL, ContactPerson = NULL, ConversionKey = NULL, DriversLicenseNumber = NULL, EducationLevelIDHighestCompleted = NULL, ExternalUniqueIdentifier = NULL, FederalEIN = NULL, FirstName = NULL, FirstNameLegal = NULL, Gender = NULL, GrandPeopleMAID = NULL, GrandPersonMAID = NULL, IsAlaskan = NULL, IsAsian = NULL, IsBlack = NULL, IsBusiness = NULL, IsEmergencyContactName = NULL, IsEmployeeName = NULL, IsFeeManagementCustomerName = NULL, IsFeeManagementPayorName = NULL, IsFoodServiceCustomerName = NULL, IsFoodServicePayorName = NULL, IsGuardianName = NULL, IsHawaiian = NULL, IsHealthProfessionalName = NULL, IsHispanic = NULL, IsInstitution = NULL, IsPayorName = NULL, IsPlaceOfEmployment = NULL, IsSingleLegalName = NULL, IsStaffName = NULL, IsStudentName = NULL, IsUserName = NULL, IsVendorName = NULL, IsWhite = NULL, LastName = NULL, LastNameLegal = NULL, MaritalStatus = NULL, MediaIDPhoto = NULL, MiddleName = NULL, MiddleNameLegal = NULL, NameIDPlaceOfEmployment = NULL, NameKey = NULL, NameSuffixID = NULL, NameSuffixIDLegal = NULL, NameTitleID = NULL, NameTitleIDLegal = NULL, OccupationID = NULL, RaceVerifiedBy = NULL, RaceVerifiedDate = NULL, SocialSecurityNumber = NULL, SpecifySeparateLegalName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "Name", objectId = NameID, body = list(DataObject = body), searchFields = append("NameID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Certifications
	#'
	#' This function returns a dataframe or json object of Certifications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Certifications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Certifications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Certification') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of Certifications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertifications <- function(searchConditionsList = NULL, CertificationID = F, CertificationNumber = F, CertificationThirdPartyImportID = F, CertificationTypeID = F, Comment = F, CreatedTime = F, ExpirationDate = F, InstitutionID = F, IssueDate = F, ModifiedTime = F, NameID = F, StateID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "Certification", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Certification
	#'
	#' This function returns a dataframe or json object of a Certification
	#' @param CertificationID The ID of the Certification to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Certification. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Certification.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Certification') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of Certification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertification <- function(CertificationID, CertificationNumber = F, CertificationThirdPartyImportID = F, CertificationTypeID = F, Comment = F, CreatedTime = F, ExpirationDate = F, InstitutionID = F, IssueDate = F, ModifiedTime = F, NameID = F, StateID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "Certification", objectId = CertificationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Certification
	#'
	#' This function deletes a Certification
	#' @param CertificationID The ID of the Certification to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationID of the deleted Certification.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertification <- function(CertificationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "Certification", objectId = CertificationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Certification
	#'
	#' This function creates a Certification
	#' @param fieldNames The field values to give the created Certification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created Certification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertification <- function(CertificationNumber = NULL, CertificationThirdPartyImportID = NULL, CertificationTypeID = NULL, Comment = NULL, ExpirationDate = NULL, InstitutionID = NULL, IssueDate = NULL, NameID = NULL, StateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "Certification", body = list(DataObject = body), searchFields = append("CertificationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Certification
	#'
	#' This function modifies a Certification
	#' @param fieldNames The field values to give the modified Certification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified Certification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertification <- function(CertificationID, CertificationNumber = NULL, CertificationThirdPartyImportID = NULL, CertificationTypeID = NULL, Comment = NULL, ExpirationDate = NULL, InstitutionID = NULL, IssueDate = NULL, NameID = NULL, StateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "Certification", objectId = CertificationID, body = list(DataObject = body), searchFields = append("CertificationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationCompetencies
	#'
	#' This function returns a dataframe or json object of CertificationCompetencies
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationCompetencies. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationCompetencies.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationCompetency') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationCompetencies
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationCompetencies <- function(searchConditionsList = NULL, CertificationCompetencyID = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationCompetency", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationCompetency
	#'
	#' This function returns a dataframe or json object of a CertificationCompetency
	#' @param CertificationCompetencyID The ID of the CertificationCompetency to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationCompetency. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationCompetency.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationCompetency') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationCompetency
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationCompetency <- function(CertificationCompetencyID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationCompetencyID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationCompetency", objectId = CertificationCompetencyID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationCompetency
	#'
	#' This function deletes a CertificationCompetency
	#' @param CertificationCompetencyID The ID of the CertificationCompetency to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationCompetencyID of the deleted CertificationCompetency.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationCompetency <- function(CertificationCompetencyID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationCompetency", objectId = CertificationCompetencyID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationCompetency
	#'
	#' This function creates a CertificationCompetency
	#' @param fieldNames The field values to give the created CertificationCompetency. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationCompetency
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationCompetency <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationCompetency", body = list(DataObject = body), searchFields = append("CertificationCompetencyID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationCompetency
	#'
	#' This function modifies a CertificationCompetency
	#' @param fieldNames The field values to give the modified CertificationCompetency. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationCompetency
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationCompetency <- function(CertificationCompetencyID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationCompetency", objectId = CertificationCompetencyID, body = list(DataObject = body), searchFields = append("CertificationCompetencyID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationDetails
	#'
	#' This function returns a dataframe or json object of CertificationDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationDetail') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationDetails <- function(searchConditionsList = NULL, CertificationCompetencyID = F, CertificationDetailID = F, CertificationGradeIDHigh = F, CertificationGradeIDLow = F, CertificationID = F, CertificationLevelID = F, CertificationSubjectID = F, CertificationThirdPartyImportID = F, CreatedTime = F, ExpirationDate = F, IsHighlyQualified = F, IssueDate = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationDetail
	#'
	#' This function returns a dataframe or json object of a CertificationDetail
	#' @param CertificationDetailID The ID of the CertificationDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationDetail <- function(CertificationDetailID, CertificationCompetencyID = F, CertificationGradeIDHigh = F, CertificationGradeIDLow = F, CertificationID = F, CertificationLevelID = F, CertificationSubjectID = F, CertificationThirdPartyImportID = F, CreatedTime = F, ExpirationDate = F, IsHighlyQualified = F, IssueDate = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationDetail", objectId = CertificationDetailID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationDetail
	#'
	#' This function deletes a CertificationDetail
	#' @param CertificationDetailID The ID of the CertificationDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationDetailID of the deleted CertificationDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationDetail <- function(CertificationDetailID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationDetail", objectId = CertificationDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationDetail
	#'
	#' This function creates a CertificationDetail
	#' @param fieldNames The field values to give the created CertificationDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationDetail <- function(CertificationCompetencyID = NULL, CertificationGradeIDHigh = NULL, CertificationGradeIDLow = NULL, CertificationID = NULL, CertificationLevelID = NULL, CertificationSubjectID = NULL, CertificationThirdPartyImportID = NULL, ExpirationDate = NULL, IsHighlyQualified = NULL, IssueDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationDetail", body = list(DataObject = body), searchFields = append("CertificationDetailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationDetail
	#'
	#' This function modifies a CertificationDetail
	#' @param fieldNames The field values to give the modified CertificationDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationDetail <- function(CertificationDetailID, CertificationCompetencyID = NULL, CertificationGradeIDHigh = NULL, CertificationGradeIDLow = NULL, CertificationID = NULL, CertificationLevelID = NULL, CertificationSubjectID = NULL, CertificationThirdPartyImportID = NULL, ExpirationDate = NULL, IsHighlyQualified = NULL, IssueDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationDetail", objectId = CertificationDetailID, body = list(DataObject = body), searchFields = append("CertificationDetailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationLevels
	#'
	#' This function returns a dataframe or json object of CertificationLevels
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationLevels. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationLevels.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationLevel') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationLevels
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationLevels <- function(searchConditionsList = NULL, CertificationLevelID = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationLevel", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationLevel
	#'
	#' This function returns a dataframe or json object of a CertificationLevel
	#' @param CertificationLevelID The ID of the CertificationLevel to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationLevel. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationLevel.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationLevel') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationLevel <- function(CertificationLevelID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationLevelID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationLevel", objectId = CertificationLevelID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationLevel
	#'
	#' This function deletes a CertificationLevel
	#' @param CertificationLevelID The ID of the CertificationLevel to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationLevelID of the deleted CertificationLevel.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationLevel <- function(CertificationLevelID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationLevel", objectId = CertificationLevelID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationLevel
	#'
	#' This function creates a CertificationLevel
	#' @param fieldNames The field values to give the created CertificationLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationLevel <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationLevel", body = list(DataObject = body), searchFields = append("CertificationLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationLevel
	#'
	#' This function modifies a CertificationLevel
	#' @param fieldNames The field values to give the modified CertificationLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationLevel <- function(CertificationLevelID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationLevel", objectId = CertificationLevelID, body = list(DataObject = body), searchFields = append("CertificationLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationSubjects
	#'
	#' This function returns a dataframe or json object of CertificationSubjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationSubjects. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationSubjects.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationSubject') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationSubjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationSubjects <- function(searchConditionsList = NULL, CertificationSubjectID = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationSubject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationSubject
	#'
	#' This function returns a dataframe or json object of a CertificationSubject
	#' @param CertificationSubjectID The ID of the CertificationSubject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationSubject. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationSubject.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationSubject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationSubject <- function(CertificationSubjectID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationSubjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationSubject", objectId = CertificationSubjectID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationSubject
	#'
	#' This function deletes a CertificationSubject
	#' @param CertificationSubjectID The ID of the CertificationSubject to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationSubjectID of the deleted CertificationSubject.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationSubject <- function(CertificationSubjectID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationSubject", objectId = CertificationSubjectID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationSubject
	#'
	#' This function creates a CertificationSubject
	#' @param fieldNames The field values to give the created CertificationSubject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationSubject <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationSubject", body = list(DataObject = body), searchFields = append("CertificationSubjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationSubject
	#'
	#' This function modifies a CertificationSubject
	#' @param fieldNames The field values to give the modified CertificationSubject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationSubject <- function(CertificationSubjectID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationSubject", objectId = CertificationSubjectID, body = list(DataObject = body), searchFields = append("CertificationSubjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationTypes
	#'
	#' This function returns a dataframe or json object of CertificationTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationType') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationTypes <- function(searchConditionsList = NULL, CertificationTypeID = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, IsCRDCCertified = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationType
	#'
	#' This function returns a dataframe or json object of a CertificationType
	#' @param CertificationTypeID The ID of the CertificationType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationType <- function(CertificationTypeID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, IsCRDCCertified = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationType", objectId = CertificationTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationType
	#'
	#' This function deletes a CertificationType
	#' @param CertificationTypeID The ID of the CertificationType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationTypeID of the deleted CertificationType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationType <- function(CertificationTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationType", objectId = CertificationTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationType
	#'
	#' This function creates a CertificationType
	#' @param fieldNames The field values to give the created CertificationType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationType <- function(Code = NULL, Description = NULL, IsCRDCCertified = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationType", body = list(DataObject = body), searchFields = append("CertificationTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationType
	#'
	#' This function modifies a CertificationType
	#' @param fieldNames The field values to give the modified CertificationType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationType <- function(CertificationTypeID, Code = NULL, Description = NULL, IsCRDCCertified = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationType", objectId = CertificationTypeID, body = list(DataObject = body), searchFields = append("CertificationTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AddressRangeDefaults
	#'
	#' This function returns a dataframe or json object of AddressRangeDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AddressRangeDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AddressRangeDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AddressRangeDefault') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of AddressRangeDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAddressRangeDefaults <- function(searchConditionsList = NULL, AddressRangeDefaultID = F, AddressRangeDefaultIDClonedFrom = F, AddressRangeDefaultIDClonedTo = F, City = F, CreatedTime = F, DefaultSchools = F, DistrictID = F, FullAddressRange = F, IsManual = F, ModifiedTime = F, SchoolPathID = F, SchoolYearID = F, StateAbbreviation = F, StreetDirection = F, StreetName = F, StreetNumberHigh = F, StreetNumberLow = F, StreetSide = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ZipCode = F, ZipCodeAddOn = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "AddressRangeDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AddressRangeDefault
	#'
	#' This function returns a dataframe or json object of an AddressRangeDefault
	#' @param AddressRangeDefaultID The ID of the AddressRangeDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AddressRangeDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AddressRangeDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AddressRangeDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of AddressRangeDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAddressRangeDefault <- function(AddressRangeDefaultID, AddressRangeDefaultIDClonedFrom = F, AddressRangeDefaultIDClonedTo = F, City = F, CreatedTime = F, DefaultSchools = F, DistrictID = F, FullAddressRange = F, IsManual = F, ModifiedTime = F, SchoolPathID = F, SchoolYearID = F, StateAbbreviation = F, StreetDirection = F, StreetName = F, StreetNumberHigh = F, StreetNumberLow = F, StreetSide = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ZipCode = F, ZipCodeAddOn = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AddressRangeDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "AddressRangeDefault", objectId = AddressRangeDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AddressRangeDefault
	#'
	#' This function deletes an AddressRangeDefault
	#' @param AddressRangeDefaultID The ID of the AddressRangeDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The AddressRangeDefaultID of the deleted AddressRangeDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAddressRangeDefault <- function(AddressRangeDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "AddressRangeDefault", objectId = AddressRangeDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AddressRangeDefault
	#'
	#' This function creates an AddressRangeDefault
	#' @param fieldNames The field values to give the created AddressRangeDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created AddressRangeDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAddressRangeDefault <- function(AddressRangeDefaultIDClonedFrom = NULL, City = NULL, DistrictID = NULL, IsManual = NULL, SchoolPathID = NULL, SchoolYearID = NULL, StateAbbreviation = NULL, StreetDirection = NULL, StreetName = NULL, StreetNumberHigh = NULL, StreetNumberLow = NULL, StreetSide = NULL, ZipCode = NULL, ZipCodeAddOn = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "AddressRangeDefault", body = list(DataObject = body), searchFields = append("AddressRangeDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AddressRangeDefault
	#'
	#' This function modifies an AddressRangeDefault
	#' @param fieldNames The field values to give the modified AddressRangeDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified AddressRangeDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAddressRangeDefault <- function(AddressRangeDefaultID, AddressRangeDefaultIDClonedFrom = NULL, City = NULL, DistrictID = NULL, IsManual = NULL, SchoolPathID = NULL, SchoolYearID = NULL, StateAbbreviation = NULL, StreetDirection = NULL, StreetName = NULL, StreetNumberHigh = NULL, StreetNumberLow = NULL, StreetSide = NULL, ZipCode = NULL, ZipCodeAddOn = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "AddressRangeDefault", objectId = AddressRangeDefaultID, body = list(DataObject = body), searchFields = append("AddressRangeDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AddressRangeDefaultAddresses
	#'
	#' This function returns a dataframe or json object of AddressRangeDefaultAddresses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AddressRangeDefaultAddresses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AddressRangeDefaultAddresses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AddressRangeDefaultAddress') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of AddressRangeDefaultAddresses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAddressRangeDefaultAddresses <- function(searchConditionsList = NULL, AddressID = F, AddressRangeDefaultAddressID = F, AddressRangeDefaultID = F, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "AddressRangeDefaultAddress", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AddressRangeDefaultAddress
	#'
	#' This function returns a dataframe or json object of an AddressRangeDefaultAddress
	#' @param AddressRangeDefaultAddressID The ID of the AddressRangeDefaultAddress to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AddressRangeDefaultAddress. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AddressRangeDefaultAddress.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AddressRangeDefaultAddress') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of AddressRangeDefaultAddress
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAddressRangeDefaultAddress <- function(AddressRangeDefaultAddressID, AddressID = F, AddressRangeDefaultID = F, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AddressRangeDefaultAddressID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "AddressRangeDefaultAddress", objectId = AddressRangeDefaultAddressID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AddressRangeDefaultAddress
	#'
	#' This function deletes an AddressRangeDefaultAddress
	#' @param AddressRangeDefaultAddressID The ID of the AddressRangeDefaultAddress to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The AddressRangeDefaultAddressID of the deleted AddressRangeDefaultAddress.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAddressRangeDefaultAddress <- function(AddressRangeDefaultAddressID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "AddressRangeDefaultAddress", objectId = AddressRangeDefaultAddressID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AddressRangeDefaultAddress
	#'
	#' This function creates an AddressRangeDefaultAddress
	#' @param fieldNames The field values to give the created AddressRangeDefaultAddress. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created AddressRangeDefaultAddress
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAddressRangeDefaultAddress <- function(AddressID = NULL, AddressRangeDefaultID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "AddressRangeDefaultAddress", body = list(DataObject = body), searchFields = append("AddressRangeDefaultAddressID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AddressRangeDefaultAddress
	#'
	#' This function modifies an AddressRangeDefaultAddress
	#' @param fieldNames The field values to give the modified AddressRangeDefaultAddress. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified AddressRangeDefaultAddress
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAddressRangeDefaultAddress <- function(AddressRangeDefaultAddressID, AddressID = NULL, AddressRangeDefaultID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "AddressRangeDefaultAddress", objectId = AddressRangeDefaultAddressID, body = list(DataObject = body), searchFields = append("AddressRangeDefaultAddressID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAddresses
	#'
	#' This function returns a dataframe or json object of TempAddresses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAddresses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAddresses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAddress') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of TempAddresses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAddresses <- function(searchConditionsList = NULL, AddressID = F, AddressUsedBy = F, CreatedTime = F, CurrentFormattedFullAddress = F, FieldPathsToUpdate = F, FieldsToUpdate = F, ModifiedTime = F, NewFormattedFullAddress = F, Selected = F, TempAddressID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowErrors = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "TempAddress", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAddress
	#'
	#' This function returns a dataframe or json object of a TempAddress
	#' @param TempAddressID The ID of the TempAddress to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAddress. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAddress.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAddress') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of TempAddress
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAddress <- function(TempAddressID, AddressID = F, AddressUsedBy = F, CreatedTime = F, CurrentFormattedFullAddress = F, FieldPathsToUpdate = F, FieldsToUpdate = F, ModifiedTime = F, NewFormattedFullAddress = F, Selected = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowErrors = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAddressID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "TempAddress", objectId = TempAddressID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAddress
	#'
	#' This function deletes a TempAddress
	#' @param TempAddressID The ID of the TempAddress to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The TempAddressID of the deleted TempAddress.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAddress <- function(TempAddressID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "TempAddress", objectId = TempAddressID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAddress
	#'
	#' This function creates a TempAddress
	#' @param fieldNames The field values to give the created TempAddress. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created TempAddress
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAddress <- function(AddressID = NULL, AddressUsedBy = NULL, CurrentFormattedFullAddress = NULL, FieldsToUpdate = NULL, NewFormattedFullAddress = NULL, WorkflowErrors = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "TempAddress", body = list(DataObject = body), searchFields = append("TempAddressID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAddress
	#'
	#' This function modifies a TempAddress
	#' @param fieldNames The field values to give the modified TempAddress. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified TempAddress
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAddress <- function(TempAddressID, AddressID = NULL, AddressUsedBy = NULL, CurrentFormattedFullAddress = NULL, FieldsToUpdate = NULL, NewFormattedFullAddress = NULL, WorkflowErrors = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "TempAddress", objectId = TempAddressID, body = list(DataObject = body), searchFields = append("TempAddressID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AddressRangeImportSchools
	#'
	#' This function returns a dataframe or json object of AddressRangeImportSchools
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AddressRangeImportSchools. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AddressRangeImportSchools.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AddressRangeImportSchool') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of AddressRangeImportSchools
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAddressRangeImportSchools <- function(searchConditionsList = NULL, AddressRangeImportSchoolID = F, AddressRangeImportSchoolIDClonedFrom = F, AddressRangeImportSchoolIDClonedTo = F, CodeDescription = F, CreatedTime = F, Description = F, ImportSchoolCode = F, ModifiedTime = F, SchoolID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "AddressRangeImportSchool", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AddressRangeImportSchool
	#'
	#' This function returns a dataframe or json object of an AddressRangeImportSchool
	#' @param AddressRangeImportSchoolID The ID of the AddressRangeImportSchool to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AddressRangeImportSchool. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AddressRangeImportSchool.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AddressRangeImportSchool') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of AddressRangeImportSchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAddressRangeImportSchool <- function(AddressRangeImportSchoolID, AddressRangeImportSchoolIDClonedFrom = F, AddressRangeImportSchoolIDClonedTo = F, CodeDescription = F, CreatedTime = F, Description = F, ImportSchoolCode = F, ModifiedTime = F, SchoolID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AddressRangeImportSchoolID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "AddressRangeImportSchool", objectId = AddressRangeImportSchoolID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AddressRangeImportSchool
	#'
	#' This function deletes an AddressRangeImportSchool
	#' @param AddressRangeImportSchoolID The ID of the AddressRangeImportSchool to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The AddressRangeImportSchoolID of the deleted AddressRangeImportSchool.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAddressRangeImportSchool <- function(AddressRangeImportSchoolID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "AddressRangeImportSchool", objectId = AddressRangeImportSchoolID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AddressRangeImportSchool
	#'
	#' This function creates an AddressRangeImportSchool
	#' @param fieldNames The field values to give the created AddressRangeImportSchool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created AddressRangeImportSchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAddressRangeImportSchool <- function(AddressRangeImportSchoolIDClonedFrom = NULL, Description = NULL, ImportSchoolCode = NULL, SchoolID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "AddressRangeImportSchool", body = list(DataObject = body), searchFields = append("AddressRangeImportSchoolID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AddressRangeImportSchool
	#'
	#' This function modifies an AddressRangeImportSchool
	#' @param fieldNames The field values to give the modified AddressRangeImportSchool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified AddressRangeImportSchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAddressRangeImportSchool <- function(AddressRangeImportSchoolID, AddressRangeImportSchoolIDClonedFrom = NULL, Description = NULL, ImportSchoolCode = NULL, SchoolID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "AddressRangeImportSchool", objectId = AddressRangeImportSchoolID, body = list(DataObject = body), searchFields = append("AddressRangeImportSchoolID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAddressRangeDefaults
	#'
	#' This function returns a dataframe or json object of TempAddressRangeDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAddressRangeDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAddressRangeDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAddressRangeDefault') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of TempAddressRangeDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAddressRangeDefaults <- function(searchConditionsList = NULL, City = F, CreatedTime = F, DefaultSchools = F, Exception = F, ModifiedTime = F, StateAbbreviation = F, StreetDirection = F, StreetName = F, StreetNumberHigh = F, StreetNumberLow = F, StreetSideCode = F, TempAddressRangeDefaultID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ZipCode = F, ZipCodeAddOn = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "TempAddressRangeDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAddressRangeDefault
	#'
	#' This function returns a dataframe or json object of a TempAddressRangeDefault
	#' @param TempAddressRangeDefaultID The ID of the TempAddressRangeDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAddressRangeDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAddressRangeDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAddressRangeDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of TempAddressRangeDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAddressRangeDefault <- function(TempAddressRangeDefaultID, City = F, CreatedTime = F, DefaultSchools = F, Exception = F, ModifiedTime = F, StateAbbreviation = F, StreetDirection = F, StreetName = F, StreetNumberHigh = F, StreetNumberLow = F, StreetSideCode = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ZipCode = F, ZipCodeAddOn = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAddressRangeDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "TempAddressRangeDefault", objectId = TempAddressRangeDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAddressRangeDefault
	#'
	#' This function deletes a TempAddressRangeDefault
	#' @param TempAddressRangeDefaultID The ID of the TempAddressRangeDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The TempAddressRangeDefaultID of the deleted TempAddressRangeDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAddressRangeDefault <- function(TempAddressRangeDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "TempAddressRangeDefault", objectId = TempAddressRangeDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAddressRangeDefault
	#'
	#' This function creates a TempAddressRangeDefault
	#' @param fieldNames The field values to give the created TempAddressRangeDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created TempAddressRangeDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAddressRangeDefault <- function(City = NULL, DefaultSchools = NULL, Exception = NULL, StateAbbreviation = NULL, StreetDirection = NULL, StreetName = NULL, StreetNumberHigh = NULL, StreetNumberLow = NULL, StreetSideCode = NULL, ZipCode = NULL, ZipCodeAddOn = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "TempAddressRangeDefault", body = list(DataObject = body), searchFields = append("TempAddressRangeDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAddressRangeDefault
	#'
	#' This function modifies a TempAddressRangeDefault
	#' @param fieldNames The field values to give the modified TempAddressRangeDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified TempAddressRangeDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAddressRangeDefault <- function(TempAddressRangeDefaultID, City = NULL, DefaultSchools = NULL, Exception = NULL, StateAbbreviation = NULL, StreetDirection = NULL, StreetName = NULL, StreetNumberHigh = NULL, StreetNumberLow = NULL, StreetSideCode = NULL, ZipCode = NULL, ZipCodeAddOn = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "TempAddressRangeDefault", objectId = TempAddressRangeDefaultID, body = list(DataObject = body), searchFields = append("TempAddressRangeDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NameWebsites
	#'
	#' This function returns a dataframe or json object of NameWebsites
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameWebsites. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameWebsites.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameWebsite') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of NameWebsites
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNameWebsites <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, NameID = F, NameWebsiteID = F, Note = F, Rank = F, URL = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "NameWebsite", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NameWebsite
	#'
	#' This function returns a dataframe or json object of a NameWebsite
	#' @param NameWebsiteID The ID of the NameWebsite to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameWebsite. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameWebsite.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameWebsite') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of NameWebsite
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNameWebsite <- function(NameWebsiteID, CreatedTime = F, ModifiedTime = F, NameID = F, Note = F, Rank = F, URL = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NameWebsiteID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "NameWebsite", objectId = NameWebsiteID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NameWebsite
	#'
	#' This function deletes a NameWebsite
	#' @param NameWebsiteID The ID of the NameWebsite to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The NameWebsiteID of the deleted NameWebsite.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNameWebsite <- function(NameWebsiteID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "NameWebsite", objectId = NameWebsiteID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NameWebsite
	#'
	#' This function creates a NameWebsite
	#' @param fieldNames The field values to give the created NameWebsite. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created NameWebsite
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNameWebsite <- function(NameID = NULL, Note = NULL, Rank = NULL, URL = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "NameWebsite", body = list(DataObject = body), searchFields = append("NameWebsiteID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NameWebsite
	#'
	#' This function modifies a NameWebsite
	#' @param fieldNames The field values to give the modified NameWebsite. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified NameWebsite
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNameWebsite <- function(NameWebsiteID, NameID = NULL, Note = NULL, Rank = NULL, URL = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "NameWebsite", objectId = NameWebsiteID, body = list(DataObject = body), searchFields = append("NameWebsiteID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempNameAddresses
	#'
	#' This function returns a dataframe or json object of TempNameAddresses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNameAddresses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNameAddresses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNameAddress') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of TempNameAddresses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempNameAddresses <- function(searchConditionsList = NULL, AddressID = F, CreatedTime = F, FullAddress = F, Is1099 = F, IsBusDropoff = F, IsBusPickup = F, IsMailing = F, IsOrderFrom = F, IsPhysical = F, IsRemitTo = F, IsW2 = F, ModifiedTime = F, NameID = F, TempNameAddressID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "TempNameAddress", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempNameAddress
	#'
	#' This function returns a dataframe or json object of a TempNameAddress
	#' @param TempNameAddressID The ID of the TempNameAddress to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNameAddress. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNameAddress.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNameAddress') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of TempNameAddress
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempNameAddress <- function(TempNameAddressID, AddressID = F, CreatedTime = F, FullAddress = F, Is1099 = F, IsBusDropoff = F, IsBusPickup = F, IsMailing = F, IsOrderFrom = F, IsPhysical = F, IsRemitTo = F, IsW2 = F, ModifiedTime = F, NameID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempNameAddressID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "TempNameAddress", objectId = TempNameAddressID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempNameAddress
	#'
	#' This function deletes a TempNameAddress
	#' @param TempNameAddressID The ID of the TempNameAddress to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The TempNameAddressID of the deleted TempNameAddress.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempNameAddress <- function(TempNameAddressID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "TempNameAddress", objectId = TempNameAddressID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempNameAddress
	#'
	#' This function creates a TempNameAddress
	#' @param fieldNames The field values to give the created TempNameAddress. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created TempNameAddress
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempNameAddress <- function(AddressID = NULL, FullAddress = NULL, Is1099 = NULL, IsBusDropoff = NULL, IsBusPickup = NULL, IsMailing = NULL, IsOrderFrom = NULL, IsPhysical = NULL, IsRemitTo = NULL, IsW2 = NULL, NameID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "TempNameAddress", body = list(DataObject = body), searchFields = append("TempNameAddressID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempNameAddress
	#'
	#' This function modifies a TempNameAddress
	#' @param fieldNames The field values to give the modified TempNameAddress. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified TempNameAddress
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempNameAddress <- function(TempNameAddressID, AddressID = NULL, FullAddress = NULL, Is1099 = NULL, IsBusDropoff = NULL, IsBusPickup = NULL, IsMailing = NULL, IsOrderFrom = NULL, IsPhysical = NULL, IsRemitTo = NULL, IsW2 = NULL, NameID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "TempNameAddress", objectId = TempNameAddressID, body = list(DataObject = body), searchFields = append("TempNameAddressID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NameAliases
	#'
	#' This function returns a dataframe or json object of NameAliases
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameAliases. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameAliases.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameAlias') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of NameAliases
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNameAliases <- function(searchConditionsList = NULL, CreatedTime = F, EffectiveDate = F, FirstName = F, FullNameFL = F, FullNameLF = F, IsBusiness = F, IsLegalName = F, IsMaidenName = F, IsSingleLegalName = F, LastName = F, MiddleName = F, ModifiedTime = F, NameAliasID = F, NameID = F, NameSuffixID = F, NameTitleID = F, Rank = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "NameAlias", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NameAlias
	#'
	#' This function returns a dataframe or json object of a NameAlias
	#' @param NameAliasID The ID of the NameAlias to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameAlias. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameAlias.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameAlias') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of NameAlias
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNameAlias <- function(NameAliasID, CreatedTime = F, EffectiveDate = F, FirstName = F, FullNameFL = F, FullNameLF = F, IsBusiness = F, IsLegalName = F, IsMaidenName = F, IsSingleLegalName = F, LastName = F, MiddleName = F, ModifiedTime = F, NameID = F, NameSuffixID = F, NameTitleID = F, Rank = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NameAliasID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "NameAlias", objectId = NameAliasID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NameAlias
	#'
	#' This function deletes a NameAlias
	#' @param NameAliasID The ID of the NameAlias to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The NameAliasID of the deleted NameAlias.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNameAlias <- function(NameAliasID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "NameAlias", objectId = NameAliasID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NameAlias
	#'
	#' This function creates a NameAlias
	#' @param fieldNames The field values to give the created NameAlias. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created NameAlias
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNameAlias <- function(EffectiveDate = NULL, FirstName = NULL, IsBusiness = NULL, IsLegalName = NULL, IsMaidenName = NULL, IsSingleLegalName = NULL, LastName = NULL, MiddleName = NULL, NameID = NULL, NameSuffixID = NULL, NameTitleID = NULL, Rank = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "NameAlias", body = list(DataObject = body), searchFields = append("NameAliasID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NameAlias
	#'
	#' This function modifies a NameAlias
	#' @param fieldNames The field values to give the modified NameAlias. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified NameAlias
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNameAlias <- function(NameAliasID, EffectiveDate = NULL, FirstName = NULL, IsBusiness = NULL, IsLegalName = NULL, IsMaidenName = NULL, IsSingleLegalName = NULL, LastName = NULL, MiddleName = NULL, NameID = NULL, NameSuffixID = NULL, NameTitleID = NULL, Rank = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "NameAlias", objectId = NameAliasID, body = list(DataObject = body), searchFields = append("NameAliasID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List LanguageSchoolYears
	#'
	#' This function returns a dataframe or json object of LanguageSchoolYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LanguageSchoolYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LanguageSchoolYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LanguageSchoolYear') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of LanguageSchoolYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLanguageSchoolYears <- function(searchConditionsList = NULL, CreatedTime = F, EdFiLanguageID = F, LanguageID = F, LanguageSchoolYearID = F, LanguageSchoolYearIDClonedFrom = F, LanguageSchoolYearMNID = F, ModifiedTime = F, SchoolYearID = F, StateHeadStartLanguageMNID = F, StateLanguageCodeMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "LanguageSchoolYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a LanguageSchoolYear
	#'
	#' This function returns a dataframe or json object of a LanguageSchoolYear
	#' @param LanguageSchoolYearID The ID of the LanguageSchoolYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LanguageSchoolYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LanguageSchoolYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LanguageSchoolYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of LanguageSchoolYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLanguageSchoolYear <- function(LanguageSchoolYearID, CreatedTime = F, EdFiLanguageID = F, LanguageID = F, LanguageSchoolYearIDClonedFrom = F, LanguageSchoolYearMNID = F, ModifiedTime = F, SchoolYearID = F, StateHeadStartLanguageMNID = F, StateLanguageCodeMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LanguageSchoolYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "LanguageSchoolYear", objectId = LanguageSchoolYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a LanguageSchoolYear
	#'
	#' This function deletes a LanguageSchoolYear
	#' @param LanguageSchoolYearID The ID of the LanguageSchoolYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The LanguageSchoolYearID of the deleted LanguageSchoolYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLanguageSchoolYear <- function(LanguageSchoolYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "LanguageSchoolYear", objectId = LanguageSchoolYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a LanguageSchoolYear
	#'
	#' This function creates a LanguageSchoolYear
	#' @param fieldNames The field values to give the created LanguageSchoolYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created LanguageSchoolYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLanguageSchoolYear <- function(EdFiLanguageID = NULL, LanguageID = NULL, LanguageSchoolYearIDClonedFrom = NULL, SchoolYearID = NULL, StateHeadStartLanguageMNID = NULL, StateLanguageCodeMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "LanguageSchoolYear", body = list(DataObject = body), searchFields = append("LanguageSchoolYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a LanguageSchoolYear
	#'
	#' This function modifies a LanguageSchoolYear
	#' @param fieldNames The field values to give the modified LanguageSchoolYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified LanguageSchoolYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLanguageSchoolYear <- function(LanguageSchoolYearID, EdFiLanguageID = NULL, LanguageID = NULL, LanguageSchoolYearIDClonedFrom = NULL, SchoolYearID = NULL, StateHeadStartLanguageMNID = NULL, StateLanguageCodeMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "LanguageSchoolYear", objectId = LanguageSchoolYearID, body = list(DataObject = body), searchFields = append("LanguageSchoolYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationThirdPartyFormats
	#'
	#' This function returns a dataframe or json object of CertificationThirdPartyFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyFormat') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationThirdPartyFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationThirdPartyFormats <- function(searchConditionsList = NULL, CertificationThirdPartyFormatID = F, Code = F, CodeDescription = F, CreatedTime = F, DateFormat = F, Description = F, DistrictID = F, ImportType = F, IsSystemLoaded = F, ModifiedTime = F, NameIdentification = F, SkywardHash = F, SkywardID = F, SkywardIDClonedFrom = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationThirdPartyFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationThirdPartyFormat
	#'
	#' This function returns a dataframe or json object of a CertificationThirdPartyFormat
	#' @param CertificationThirdPartyFormatID The ID of the CertificationThirdPartyFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationThirdPartyFormat <- function(CertificationThirdPartyFormatID, Code = F, CodeDescription = F, CreatedTime = F, DateFormat = F, Description = F, DistrictID = F, ImportType = F, IsSystemLoaded = F, ModifiedTime = F, NameIdentification = F, SkywardHash = F, SkywardID = F, SkywardIDClonedFrom = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationThirdPartyFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormat", objectId = CertificationThirdPartyFormatID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationThirdPartyFormat
	#'
	#' This function deletes a CertificationThirdPartyFormat
	#' @param CertificationThirdPartyFormatID The ID of the CertificationThirdPartyFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationThirdPartyFormatID of the deleted CertificationThirdPartyFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationThirdPartyFormat <- function(CertificationThirdPartyFormatID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormat", objectId = CertificationThirdPartyFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationThirdPartyFormat
	#'
	#' This function creates a CertificationThirdPartyFormat
	#' @param fieldNames The field values to give the created CertificationThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationThirdPartyFormat <- function(Code = NULL, DateFormat = NULL, Description = NULL, DistrictID = NULL, ImportType = NULL, NameIdentification = NULL, SkywardIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormat", body = list(DataObject = body), searchFields = append("CertificationThirdPartyFormatID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationThirdPartyFormat
	#'
	#' This function modifies a CertificationThirdPartyFormat
	#' @param fieldNames The field values to give the modified CertificationThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationThirdPartyFormat <- function(CertificationThirdPartyFormatID, Code = NULL, DateFormat = NULL, Description = NULL, DistrictID = NULL, ImportType = NULL, NameIdentification = NULL, SkywardIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormat", objectId = CertificationThirdPartyFormatID, body = list(DataObject = body), searchFields = append("CertificationThirdPartyFormatID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InstitutionDefaults
	#'
	#' This function returns a dataframe or json object of InstitutionDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InstitutionDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InstitutionDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InstitutionDefault') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of InstitutionDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInstitutionDefaults <- function(searchConditionsList = NULL, CEEBCode = F, CreatedTime = F, InstitutionDefaultID = F, InstitutionDefaultMNID = F, MCCCCollegeCode = F, ModifiedTime = F, Name = F, SkywardHash = F, SkywardID = F, StateID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "InstitutionDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InstitutionDefault
	#'
	#' This function returns a dataframe or json object of an InstitutionDefault
	#' @param InstitutionDefaultID The ID of the InstitutionDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InstitutionDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InstitutionDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InstitutionDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of InstitutionDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInstitutionDefault <- function(InstitutionDefaultID, CEEBCode = F, CreatedTime = F, InstitutionDefaultMNID = F, MCCCCollegeCode = F, ModifiedTime = F, Name = F, SkywardHash = F, SkywardID = F, StateID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InstitutionDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "InstitutionDefault", objectId = InstitutionDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InstitutionDefault
	#'
	#' This function deletes an InstitutionDefault
	#' @param InstitutionDefaultID The ID of the InstitutionDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The InstitutionDefaultID of the deleted InstitutionDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInstitutionDefault <- function(InstitutionDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "InstitutionDefault", objectId = InstitutionDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InstitutionDefault
	#'
	#' This function creates an InstitutionDefault
	#' @param fieldNames The field values to give the created InstitutionDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created InstitutionDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInstitutionDefault <- function(CEEBCode = NULL, MCCCCollegeCode = NULL, Name = NULL, StateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "InstitutionDefault", body = list(DataObject = body), searchFields = append("InstitutionDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InstitutionDefault
	#'
	#' This function modifies an InstitutionDefault
	#' @param fieldNames The field values to give the modified InstitutionDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified InstitutionDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInstitutionDefault <- function(InstitutionDefaultID, CEEBCode = NULL, MCCCCollegeCode = NULL, Name = NULL, StateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "InstitutionDefault", objectId = InstitutionDefaultID, body = list(DataObject = body), searchFields = append("InstitutionDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationThirdPartyFormatInstitutions
	#'
	#' This function returns a dataframe or json object of CertificationThirdPartyFormatInstitutions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyFormatInstitutions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyFormatInstitutions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyFormatInstitution') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationThirdPartyFormatInstitutions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationThirdPartyFormatInstitutions <- function(searchConditionsList = NULL, CertificationThirdPartyFormatID = F, CertificationThirdPartyFormatInstitutionID = F, CreatedTime = F, ImportValue = F, InstitutionID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationThirdPartyFormatInstitution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationThirdPartyFormatInstitution
	#'
	#' This function returns a dataframe or json object of a CertificationThirdPartyFormatInstitution
	#' @param CertificationThirdPartyFormatInstitutionID The ID of the CertificationThirdPartyFormatInstitution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyFormatInstitution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyFormatInstitution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyFormatInstitution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationThirdPartyFormatInstitution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationThirdPartyFormatInstitution <- function(CertificationThirdPartyFormatInstitutionID, CertificationThirdPartyFormatID = F, CreatedTime = F, ImportValue = F, InstitutionID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationThirdPartyFormatInstitutionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatInstitution", objectId = CertificationThirdPartyFormatInstitutionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationThirdPartyFormatInstitution
	#'
	#' This function deletes a CertificationThirdPartyFormatInstitution
	#' @param CertificationThirdPartyFormatInstitutionID The ID of the CertificationThirdPartyFormatInstitution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationThirdPartyFormatInstitutionID of the deleted CertificationThirdPartyFormatInstitution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationThirdPartyFormatInstitution <- function(CertificationThirdPartyFormatInstitutionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatInstitution", objectId = CertificationThirdPartyFormatInstitutionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationThirdPartyFormatInstitution
	#'
	#' This function creates a CertificationThirdPartyFormatInstitution
	#' @param fieldNames The field values to give the created CertificationThirdPartyFormatInstitution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationThirdPartyFormatInstitution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationThirdPartyFormatInstitution <- function(CertificationThirdPartyFormatID = NULL, ImportValue = NULL, InstitutionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatInstitution", body = list(DataObject = body), searchFields = append("CertificationThirdPartyFormatInstitutionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationThirdPartyFormatInstitution
	#'
	#' This function modifies a CertificationThirdPartyFormatInstitution
	#' @param fieldNames The field values to give the modified CertificationThirdPartyFormatInstitution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationThirdPartyFormatInstitution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationThirdPartyFormatInstitution <- function(CertificationThirdPartyFormatInstitutionID, CertificationThirdPartyFormatID = NULL, ImportValue = NULL, InstitutionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatInstitution", objectId = CertificationThirdPartyFormatInstitutionID, body = list(DataObject = body), searchFields = append("CertificationThirdPartyFormatInstitutionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationDelimitedFileFormats
	#'
	#' This function returns a dataframe or json object of CertificationDelimitedFileFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationDelimitedFileFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationDelimitedFileFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationDelimitedFileFormat') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationDelimitedFileFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationDelimitedFileFormats <- function(searchConditionsList = NULL, CertificationCompetencyColumnNumber = F, CertificationDelimitedFileFormatID = F, CertificationDetailExpirationDateColumnNumber = F, CertificationDetailIssueDateColumnNumber = F, CertificationLevelColumnNumber = F, CertificationNumberColumnNumber = F, CertificationSubjectColumnNumber = F, CertificationThirdPartyFormatID = F, CertificationTypeColumnNumber = F, CommentColumnNumber = F, CreatedTime = F, DelimiterCharacter = F, DelimiterType = F, EmployeeColumnNumber = F, ExpirationDateColumnNumber = F, HighCertificationGradeColumnNumber = F, HighlyQualifiedColumnNumber = F, InstitutionNameColumnNumber = F, IssueDateColumnNumber = F, LowCertificationGradeColumnNumber = F, ModifiedTime = F, NumberOfHeaderRows = F, SkywardHash = F, SkywardID = F, StateColumnNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationDelimitedFileFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationDelimitedFileFormat
	#'
	#' This function returns a dataframe or json object of a CertificationDelimitedFileFormat
	#' @param CertificationDelimitedFileFormatID The ID of the CertificationDelimitedFileFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationDelimitedFileFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationDelimitedFileFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationDelimitedFileFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationDelimitedFileFormat <- function(CertificationDelimitedFileFormatID, CertificationCompetencyColumnNumber = F, CertificationDetailExpirationDateColumnNumber = F, CertificationDetailIssueDateColumnNumber = F, CertificationLevelColumnNumber = F, CertificationNumberColumnNumber = F, CertificationSubjectColumnNumber = F, CertificationThirdPartyFormatID = F, CertificationTypeColumnNumber = F, CommentColumnNumber = F, CreatedTime = F, DelimiterCharacter = F, DelimiterType = F, EmployeeColumnNumber = F, ExpirationDateColumnNumber = F, HighCertificationGradeColumnNumber = F, HighlyQualifiedColumnNumber = F, InstitutionNameColumnNumber = F, IssueDateColumnNumber = F, LowCertificationGradeColumnNumber = F, ModifiedTime = F, NumberOfHeaderRows = F, SkywardHash = F, SkywardID = F, StateColumnNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationDelimitedFileFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationDelimitedFileFormat", objectId = CertificationDelimitedFileFormatID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationDelimitedFileFormat
	#'
	#' This function deletes a CertificationDelimitedFileFormat
	#' @param CertificationDelimitedFileFormatID The ID of the CertificationDelimitedFileFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationDelimitedFileFormatID of the deleted CertificationDelimitedFileFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationDelimitedFileFormat <- function(CertificationDelimitedFileFormatID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationDelimitedFileFormat", objectId = CertificationDelimitedFileFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationDelimitedFileFormat
	#'
	#' This function creates a CertificationDelimitedFileFormat
	#' @param fieldNames The field values to give the created CertificationDelimitedFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationDelimitedFileFormat <- function(CertificationCompetencyColumnNumber = NULL, CertificationDetailExpirationDateColumnNumber = NULL, CertificationDetailIssueDateColumnNumber = NULL, CertificationLevelColumnNumber = NULL, CertificationNumberColumnNumber = NULL, CertificationSubjectColumnNumber = NULL, CertificationThirdPartyFormatID = NULL, CertificationTypeColumnNumber = NULL, CommentColumnNumber = NULL, DelimiterCharacter = NULL, DelimiterType = NULL, EmployeeColumnNumber = NULL, ExpirationDateColumnNumber = NULL, HighCertificationGradeColumnNumber = NULL, HighlyQualifiedColumnNumber = NULL, InstitutionNameColumnNumber = NULL, IssueDateColumnNumber = NULL, LowCertificationGradeColumnNumber = NULL, NumberOfHeaderRows = NULL, StateColumnNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationDelimitedFileFormat", body = list(DataObject = body), searchFields = append("CertificationDelimitedFileFormatID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationDelimitedFileFormat
	#'
	#' This function modifies a CertificationDelimitedFileFormat
	#' @param fieldNames The field values to give the modified CertificationDelimitedFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationDelimitedFileFormat <- function(CertificationDelimitedFileFormatID, CertificationCompetencyColumnNumber = NULL, CertificationDetailExpirationDateColumnNumber = NULL, CertificationDetailIssueDateColumnNumber = NULL, CertificationLevelColumnNumber = NULL, CertificationNumberColumnNumber = NULL, CertificationSubjectColumnNumber = NULL, CertificationThirdPartyFormatID = NULL, CertificationTypeColumnNumber = NULL, CommentColumnNumber = NULL, DelimiterCharacter = NULL, DelimiterType = NULL, EmployeeColumnNumber = NULL, ExpirationDateColumnNumber = NULL, HighCertificationGradeColumnNumber = NULL, HighlyQualifiedColumnNumber = NULL, InstitutionNameColumnNumber = NULL, IssueDateColumnNumber = NULL, LowCertificationGradeColumnNumber = NULL, NumberOfHeaderRows = NULL, StateColumnNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationDelimitedFileFormat", objectId = CertificationDelimitedFileFormatID, body = list(DataObject = body), searchFields = append("CertificationDelimitedFileFormatID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationThirdPartyFormatCertificationSubjects
	#'
	#' This function returns a dataframe or json object of CertificationThirdPartyFormatCertificationSubjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyFormatCertificationSubjects. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyFormatCertificationSubjects.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyFormatCertificationSubject') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationThirdPartyFormatCertificationSubjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationThirdPartyFormatCertificationSubjects <- function(searchConditionsList = NULL, CertificationSubjectID = F, CertificationThirdPartyFormatCertificationSubjectID = F, CertificationThirdPartyFormatID = F, CreatedTime = F, ImportValue = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationSubject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationThirdPartyFormatCertificationSubject
	#'
	#' This function returns a dataframe or json object of a CertificationThirdPartyFormatCertificationSubject
	#' @param CertificationThirdPartyFormatCertificationSubjectID The ID of the CertificationThirdPartyFormatCertificationSubject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyFormatCertificationSubject. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyFormatCertificationSubject.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyFormatCertificationSubject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationThirdPartyFormatCertificationSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationThirdPartyFormatCertificationSubject <- function(CertificationThirdPartyFormatCertificationSubjectID, CertificationSubjectID = F, CertificationThirdPartyFormatID = F, CreatedTime = F, ImportValue = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationThirdPartyFormatCertificationSubjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationSubject", objectId = CertificationThirdPartyFormatCertificationSubjectID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationThirdPartyFormatCertificationSubject
	#'
	#' This function deletes a CertificationThirdPartyFormatCertificationSubject
	#' @param CertificationThirdPartyFormatCertificationSubjectID The ID of the CertificationThirdPartyFormatCertificationSubject to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationThirdPartyFormatCertificationSubjectID of the deleted CertificationThirdPartyFormatCertificationSubject.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationThirdPartyFormatCertificationSubject <- function(CertificationThirdPartyFormatCertificationSubjectID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationSubject", objectId = CertificationThirdPartyFormatCertificationSubjectID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationThirdPartyFormatCertificationSubject
	#'
	#' This function creates a CertificationThirdPartyFormatCertificationSubject
	#' @param fieldNames The field values to give the created CertificationThirdPartyFormatCertificationSubject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationThirdPartyFormatCertificationSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationThirdPartyFormatCertificationSubject <- function(CertificationSubjectID = NULL, CertificationThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationSubject", body = list(DataObject = body), searchFields = append("CertificationThirdPartyFormatCertificationSubjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationThirdPartyFormatCertificationSubject
	#'
	#' This function modifies a CertificationThirdPartyFormatCertificationSubject
	#' @param fieldNames The field values to give the modified CertificationThirdPartyFormatCertificationSubject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationThirdPartyFormatCertificationSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationThirdPartyFormatCertificationSubject <- function(CertificationThirdPartyFormatCertificationSubjectID, CertificationSubjectID = NULL, CertificationThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationSubject", objectId = CertificationThirdPartyFormatCertificationSubjectID, body = list(DataObject = body), searchFields = append("CertificationThirdPartyFormatCertificationSubjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationThirdPartyFormatCertificationTypes
	#'
	#' This function returns a dataframe or json object of CertificationThirdPartyFormatCertificationTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyFormatCertificationTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyFormatCertificationTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyFormatCertificationType') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationThirdPartyFormatCertificationTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationThirdPartyFormatCertificationTypes <- function(searchConditionsList = NULL, CertificationThirdPartyFormatCertificationTypeID = F, CertificationThirdPartyFormatID = F, CertificationTypeID = F, CreatedTime = F, ImportValue = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationThirdPartyFormatCertificationType
	#'
	#' This function returns a dataframe or json object of a CertificationThirdPartyFormatCertificationType
	#' @param CertificationThirdPartyFormatCertificationTypeID The ID of the CertificationThirdPartyFormatCertificationType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyFormatCertificationType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyFormatCertificationType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyFormatCertificationType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationThirdPartyFormatCertificationType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationThirdPartyFormatCertificationType <- function(CertificationThirdPartyFormatCertificationTypeID, CertificationThirdPartyFormatID = F, CertificationTypeID = F, CreatedTime = F, ImportValue = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationThirdPartyFormatCertificationTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationType", objectId = CertificationThirdPartyFormatCertificationTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationThirdPartyFormatCertificationType
	#'
	#' This function deletes a CertificationThirdPartyFormatCertificationType
	#' @param CertificationThirdPartyFormatCertificationTypeID The ID of the CertificationThirdPartyFormatCertificationType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationThirdPartyFormatCertificationTypeID of the deleted CertificationThirdPartyFormatCertificationType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationThirdPartyFormatCertificationType <- function(CertificationThirdPartyFormatCertificationTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationType", objectId = CertificationThirdPartyFormatCertificationTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationThirdPartyFormatCertificationType
	#'
	#' This function creates a CertificationThirdPartyFormatCertificationType
	#' @param fieldNames The field values to give the created CertificationThirdPartyFormatCertificationType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationThirdPartyFormatCertificationType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationThirdPartyFormatCertificationType <- function(CertificationThirdPartyFormatID = NULL, CertificationTypeID = NULL, ImportValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationType", body = list(DataObject = body), searchFields = append("CertificationThirdPartyFormatCertificationTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationThirdPartyFormatCertificationType
	#'
	#' This function modifies a CertificationThirdPartyFormatCertificationType
	#' @param fieldNames The field values to give the modified CertificationThirdPartyFormatCertificationType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationThirdPartyFormatCertificationType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationThirdPartyFormatCertificationType <- function(CertificationThirdPartyFormatCertificationTypeID, CertificationThirdPartyFormatID = NULL, CertificationTypeID = NULL, ImportValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationType", objectId = CertificationThirdPartyFormatCertificationTypeID, body = list(DataObject = body), searchFields = append("CertificationThirdPartyFormatCertificationTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationThirdPartyFormatCertificationLevels
	#'
	#' This function returns a dataframe or json object of CertificationThirdPartyFormatCertificationLevels
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyFormatCertificationLevels. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyFormatCertificationLevels.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyFormatCertificationLevel') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationThirdPartyFormatCertificationLevels
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationThirdPartyFormatCertificationLevels <- function(searchConditionsList = NULL, CertificationLevelID = F, CertificationThirdPartyFormatCertificationLevelID = F, CertificationThirdPartyFormatID = F, CreatedTime = F, ImportValue = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationLevel", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationThirdPartyFormatCertificationLevel
	#'
	#' This function returns a dataframe or json object of a CertificationThirdPartyFormatCertificationLevel
	#' @param CertificationThirdPartyFormatCertificationLevelID The ID of the CertificationThirdPartyFormatCertificationLevel to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyFormatCertificationLevel. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyFormatCertificationLevel.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyFormatCertificationLevel') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationThirdPartyFormatCertificationLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationThirdPartyFormatCertificationLevel <- function(CertificationThirdPartyFormatCertificationLevelID, CertificationLevelID = F, CertificationThirdPartyFormatID = F, CreatedTime = F, ImportValue = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationThirdPartyFormatCertificationLevelID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationLevel", objectId = CertificationThirdPartyFormatCertificationLevelID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationThirdPartyFormatCertificationLevel
	#'
	#' This function deletes a CertificationThirdPartyFormatCertificationLevel
	#' @param CertificationThirdPartyFormatCertificationLevelID The ID of the CertificationThirdPartyFormatCertificationLevel to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationThirdPartyFormatCertificationLevelID of the deleted CertificationThirdPartyFormatCertificationLevel.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationThirdPartyFormatCertificationLevel <- function(CertificationThirdPartyFormatCertificationLevelID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationLevel", objectId = CertificationThirdPartyFormatCertificationLevelID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationThirdPartyFormatCertificationLevel
	#'
	#' This function creates a CertificationThirdPartyFormatCertificationLevel
	#' @param fieldNames The field values to give the created CertificationThirdPartyFormatCertificationLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationThirdPartyFormatCertificationLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationThirdPartyFormatCertificationLevel <- function(CertificationLevelID = NULL, CertificationThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationLevel", body = list(DataObject = body), searchFields = append("CertificationThirdPartyFormatCertificationLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationThirdPartyFormatCertificationLevel
	#'
	#' This function modifies a CertificationThirdPartyFormatCertificationLevel
	#' @param fieldNames The field values to give the modified CertificationThirdPartyFormatCertificationLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationThirdPartyFormatCertificationLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationThirdPartyFormatCertificationLevel <- function(CertificationThirdPartyFormatCertificationLevelID, CertificationLevelID = NULL, CertificationThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationLevel", objectId = CertificationThirdPartyFormatCertificationLevelID, body = list(DataObject = body), searchFields = append("CertificationThirdPartyFormatCertificationLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationThirdPartyFormatCertificationCompetencies
	#'
	#' This function returns a dataframe or json object of CertificationThirdPartyFormatCertificationCompetencies
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyFormatCertificationCompetencies. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyFormatCertificationCompetencies.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyFormatCertificationCompetency') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationThirdPartyFormatCertificationCompetencies
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationThirdPartyFormatCertificationCompetencies <- function(searchConditionsList = NULL, CertificationCompetencyID = F, CertificationThirdPartyFormatCertificationCompetencyID = F, CertificationThirdPartyFormatID = F, CreatedTime = F, ImportValue = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationCompetency", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationThirdPartyFormatCertificationCompetency
	#'
	#' This function returns a dataframe or json object of a CertificationThirdPartyFormatCertificationCompetency
	#' @param CertificationThirdPartyFormatCertificationCompetencyID The ID of the CertificationThirdPartyFormatCertificationCompetency to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyFormatCertificationCompetency. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyFormatCertificationCompetency.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyFormatCertificationCompetency') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationThirdPartyFormatCertificationCompetency
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationThirdPartyFormatCertificationCompetency <- function(CertificationThirdPartyFormatCertificationCompetencyID, CertificationCompetencyID = F, CertificationThirdPartyFormatID = F, CreatedTime = F, ImportValue = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationThirdPartyFormatCertificationCompetencyID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationCompetency", objectId = CertificationThirdPartyFormatCertificationCompetencyID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationThirdPartyFormatCertificationCompetency
	#'
	#' This function deletes a CertificationThirdPartyFormatCertificationCompetency
	#' @param CertificationThirdPartyFormatCertificationCompetencyID The ID of the CertificationThirdPartyFormatCertificationCompetency to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationThirdPartyFormatCertificationCompetencyID of the deleted CertificationThirdPartyFormatCertificationCompetency.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationThirdPartyFormatCertificationCompetency <- function(CertificationThirdPartyFormatCertificationCompetencyID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationCompetency", objectId = CertificationThirdPartyFormatCertificationCompetencyID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationThirdPartyFormatCertificationCompetency
	#'
	#' This function creates a CertificationThirdPartyFormatCertificationCompetency
	#' @param fieldNames The field values to give the created CertificationThirdPartyFormatCertificationCompetency. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationThirdPartyFormatCertificationCompetency
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationThirdPartyFormatCertificationCompetency <- function(CertificationCompetencyID = NULL, CertificationThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationCompetency", body = list(DataObject = body), searchFields = append("CertificationThirdPartyFormatCertificationCompetencyID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationThirdPartyFormatCertificationCompetency
	#'
	#' This function modifies a CertificationThirdPartyFormatCertificationCompetency
	#' @param fieldNames The field values to give the modified CertificationThirdPartyFormatCertificationCompetency. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationThirdPartyFormatCertificationCompetency
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationThirdPartyFormatCertificationCompetency <- function(CertificationThirdPartyFormatCertificationCompetencyID, CertificationCompetencyID = NULL, CertificationThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationCompetency", objectId = CertificationThirdPartyFormatCertificationCompetencyID, body = list(DataObject = body), searchFields = append("CertificationThirdPartyFormatCertificationCompetencyID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationThirdPartyImports
	#'
	#' This function returns a dataframe or json object of CertificationThirdPartyImports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyImports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyImports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyImport') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationThirdPartyImports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationThirdPartyImports <- function(searchConditionsList = NULL, CertificationThirdPartyFormatID = F, CertificationThirdPartyImportID = F, CreatedTime = F, ImportTime = F, MediaID = F, MediaIDFailedResult = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationThirdPartyImport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationThirdPartyImport
	#'
	#' This function returns a dataframe or json object of a CertificationThirdPartyImport
	#' @param CertificationThirdPartyImportID The ID of the CertificationThirdPartyImport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyImport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyImport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyImport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationThirdPartyImport <- function(CertificationThirdPartyImportID, CertificationThirdPartyFormatID = F, CreatedTime = F, ImportTime = F, MediaID = F, MediaIDFailedResult = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationThirdPartyImportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationThirdPartyImport", objectId = CertificationThirdPartyImportID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationThirdPartyImport
	#'
	#' This function deletes a CertificationThirdPartyImport
	#' @param CertificationThirdPartyImportID The ID of the CertificationThirdPartyImport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationThirdPartyImportID of the deleted CertificationThirdPartyImport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationThirdPartyImport <- function(CertificationThirdPartyImportID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationThirdPartyImport", objectId = CertificationThirdPartyImportID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationThirdPartyImport
	#'
	#' This function creates a CertificationThirdPartyImport
	#' @param fieldNames The field values to give the created CertificationThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationThirdPartyImport <- function(CertificationThirdPartyFormatID = NULL, ImportTime = NULL, MediaID = NULL, MediaIDFailedResult = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationThirdPartyImport", body = list(DataObject = body), searchFields = append("CertificationThirdPartyImportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationThirdPartyImport
	#'
	#' This function modifies a CertificationThirdPartyImport
	#' @param fieldNames The field values to give the modified CertificationThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationThirdPartyImport <- function(CertificationThirdPartyImportID, CertificationThirdPartyFormatID = NULL, ImportTime = NULL, MediaID = NULL, MediaIDFailedResult = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationThirdPartyImport", objectId = CertificationThirdPartyImportID, body = list(DataObject = body), searchFields = append("CertificationThirdPartyImportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationFixedLengthFileFormats
	#'
	#' This function returns a dataframe or json object of CertificationFixedLengthFileFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationFixedLengthFileFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationFixedLengthFileFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationFixedLengthFileFormat') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationFixedLengthFileFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationFixedLengthFileFormats <- function(searchConditionsList = NULL, CertificationCompetencyLength = F, CertificationCompetencyStartPosition = F, CertificationDetailExpirationDateLength = F, CertificationDetailExpirationDateStartPosition = F, CertificationDetailIssueDateLength = F, CertificationDetailIssueDateStartPosition = F, CertificationFixedLengthFileFormatID = F, CertificationLevelLength = F, CertificationLevelStartPosition = F, CertificationNumberLength = F, CertificationNumberStartPosition = F, CertificationSubjectLength = F, CertificationSubjectStartPosition = F, CertificationThirdPartyFormatID = F, CertificationTypeLength = F, CertificationTypeStartPosition = F, CommentLength = F, CommentStartPosition = F, CreatedTime = F, EmployeeLength = F, EmployeeStartPosition = F, ExpirationDateLength = F, ExpirationDateStartPosition = F, HighCertificationGradeLength = F, HighCertificationGradeStartPosition = F, HighlyQualifiedLength = F, HighlyQualifiedStartPosition = F, InstitutionNameLength = F, InstitutionNameStartPosition = F, IssueDateLength = F, IssueDateStartPosition = F, LowCertificationGradeLength = F, LowCertificationGradeStartPosition = F, ModifiedTime = F, NumberOfHeaderRows = F, SkywardHash = F, SkywardID = F, StateLength = F, StateStartPosition = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationFixedLengthFileFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationFixedLengthFileFormat
	#'
	#' This function returns a dataframe or json object of a CertificationFixedLengthFileFormat
	#' @param CertificationFixedLengthFileFormatID The ID of the CertificationFixedLengthFileFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationFixedLengthFileFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationFixedLengthFileFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationFixedLengthFileFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationFixedLengthFileFormat <- function(CertificationFixedLengthFileFormatID, CertificationCompetencyLength = F, CertificationCompetencyStartPosition = F, CertificationDetailExpirationDateLength = F, CertificationDetailExpirationDateStartPosition = F, CertificationDetailIssueDateLength = F, CertificationDetailIssueDateStartPosition = F, CertificationLevelLength = F, CertificationLevelStartPosition = F, CertificationNumberLength = F, CertificationNumberStartPosition = F, CertificationSubjectLength = F, CertificationSubjectStartPosition = F, CertificationThirdPartyFormatID = F, CertificationTypeLength = F, CertificationTypeStartPosition = F, CommentLength = F, CommentStartPosition = F, CreatedTime = F, EmployeeLength = F, EmployeeStartPosition = F, ExpirationDateLength = F, ExpirationDateStartPosition = F, HighCertificationGradeLength = F, HighCertificationGradeStartPosition = F, HighlyQualifiedLength = F, HighlyQualifiedStartPosition = F, InstitutionNameLength = F, InstitutionNameStartPosition = F, IssueDateLength = F, IssueDateStartPosition = F, LowCertificationGradeLength = F, LowCertificationGradeStartPosition = F, ModifiedTime = F, NumberOfHeaderRows = F, SkywardHash = F, SkywardID = F, StateLength = F, StateStartPosition = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationFixedLengthFileFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationFixedLengthFileFormat", objectId = CertificationFixedLengthFileFormatID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationFixedLengthFileFormat
	#'
	#' This function deletes a CertificationFixedLengthFileFormat
	#' @param CertificationFixedLengthFileFormatID The ID of the CertificationFixedLengthFileFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationFixedLengthFileFormatID of the deleted CertificationFixedLengthFileFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationFixedLengthFileFormat <- function(CertificationFixedLengthFileFormatID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationFixedLengthFileFormat", objectId = CertificationFixedLengthFileFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationFixedLengthFileFormat
	#'
	#' This function creates a CertificationFixedLengthFileFormat
	#' @param fieldNames The field values to give the created CertificationFixedLengthFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationFixedLengthFileFormat <- function(CertificationCompetencyLength = NULL, CertificationCompetencyStartPosition = NULL, CertificationDetailExpirationDateLength = NULL, CertificationDetailExpirationDateStartPosition = NULL, CertificationDetailIssueDateLength = NULL, CertificationDetailIssueDateStartPosition = NULL, CertificationLevelLength = NULL, CertificationLevelStartPosition = NULL, CertificationNumberLength = NULL, CertificationNumberStartPosition = NULL, CertificationSubjectLength = NULL, CertificationSubjectStartPosition = NULL, CertificationThirdPartyFormatID = NULL, CertificationTypeLength = NULL, CertificationTypeStartPosition = NULL, CommentLength = NULL, CommentStartPosition = NULL, EmployeeLength = NULL, EmployeeStartPosition = NULL, ExpirationDateLength = NULL, ExpirationDateStartPosition = NULL, HighCertificationGradeLength = NULL, HighCertificationGradeStartPosition = NULL, HighlyQualifiedLength = NULL, HighlyQualifiedStartPosition = NULL, InstitutionNameLength = NULL, InstitutionNameStartPosition = NULL, IssueDateLength = NULL, IssueDateStartPosition = NULL, LowCertificationGradeLength = NULL, LowCertificationGradeStartPosition = NULL, NumberOfHeaderRows = NULL, StateLength = NULL, StateStartPosition = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationFixedLengthFileFormat", body = list(DataObject = body), searchFields = append("CertificationFixedLengthFileFormatID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationFixedLengthFileFormat
	#'
	#' This function modifies a CertificationFixedLengthFileFormat
	#' @param fieldNames The field values to give the modified CertificationFixedLengthFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationFixedLengthFileFormat <- function(CertificationFixedLengthFileFormatID, CertificationCompetencyLength = NULL, CertificationCompetencyStartPosition = NULL, CertificationDetailExpirationDateLength = NULL, CertificationDetailExpirationDateStartPosition = NULL, CertificationDetailIssueDateLength = NULL, CertificationDetailIssueDateStartPosition = NULL, CertificationLevelLength = NULL, CertificationLevelStartPosition = NULL, CertificationNumberLength = NULL, CertificationNumberStartPosition = NULL, CertificationSubjectLength = NULL, CertificationSubjectStartPosition = NULL, CertificationThirdPartyFormatID = NULL, CertificationTypeLength = NULL, CertificationTypeStartPosition = NULL, CommentLength = NULL, CommentStartPosition = NULL, EmployeeLength = NULL, EmployeeStartPosition = NULL, ExpirationDateLength = NULL, ExpirationDateStartPosition = NULL, HighCertificationGradeLength = NULL, HighCertificationGradeStartPosition = NULL, HighlyQualifiedLength = NULL, HighlyQualifiedStartPosition = NULL, InstitutionNameLength = NULL, InstitutionNameStartPosition = NULL, IssueDateLength = NULL, IssueDateStartPosition = NULL, LowCertificationGradeLength = NULL, LowCertificationGradeStartPosition = NULL, NumberOfHeaderRows = NULL, StateLength = NULL, StateStartPosition = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationFixedLengthFileFormat", objectId = CertificationFixedLengthFileFormatID, body = list(DataObject = body), searchFields = append("CertificationFixedLengthFileFormatID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Counties
	#'
	#' This function returns a dataframe or json object of Counties
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Counties. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Counties.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('County') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of Counties
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCounties <- function(searchConditionsList = NULL, CountyID = F, CountyMNID = F, CreatedTime = F, ModifiedTime = F, Name = F, StateCountyMNID = F, StateID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "County", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a County
	#'
	#' This function returns a dataframe or json object of a County
	#' @param CountyID The ID of the County to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given County. Defaults to FALSE for all return fields which, for convenience, returns all fields for the County.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('County') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of County
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCounty <- function(CountyID, CountyMNID = F, CreatedTime = F, ModifiedTime = F, Name = F, StateCountyMNID = F, StateID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CountyID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "County", objectId = CountyID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a County
	#'
	#' This function deletes a County
	#' @param CountyID The ID of the County to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CountyID of the deleted County.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCounty <- function(CountyID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "County", objectId = CountyID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a County
	#'
	#' This function creates a County
	#' @param fieldNames The field values to give the created County. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created County
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCounty <- function(Name = NULL, StateCountyMNID = NULL, StateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "County", body = list(DataObject = body), searchFields = append("CountyID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a County
	#'
	#' This function modifies a County
	#' @param fieldNames The field values to give the modified County. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified County
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCounty <- function(CountyID, Name = NULL, StateCountyMNID = NULL, StateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "County", objectId = CountyID, body = list(DataObject = body), searchFields = append("CountyID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DemographicsTempCertifications
	#'
	#' This function returns a dataframe or json object of DemographicsTempCertifications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DemographicsTempCertifications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DemographicsTempCertifications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DemographicsTempCertification') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of DemographicsTempCertifications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDemographicsTempCertifications <- function(searchConditionsList = NULL, CertificationID = F, CertificationNumber = F, CertificationTypeCode = F, CertificationTypeCodeDescription = F, CertificationTypeID = F, Comment = F, CreatedTime = F, ErrorCount = F, ExpirationDate = F, FullNameLFM = F, HasErrors = F, InstitutionID = F, InstitutionName = F, IssueDate = F, LineNumber = F, ModifiedTime = F, NameID = F, StateDisplayName = F, StateID = F, TempCertificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "TempCertification", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DemographicsTempCertification
	#'
	#' This function returns a dataframe or json object of a DemographicsTempCertification
	#' @param DemographicsTempCertificationID The ID of the DemographicsTempCertification to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DemographicsTempCertification. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DemographicsTempCertification.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DemographicsTempCertification') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of DemographicsTempCertification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDemographicsTempCertification <- function(DemographicsTempCertificationID, CertificationID = F, CertificationNumber = F, CertificationTypeCode = F, CertificationTypeCodeDescription = F, CertificationTypeID = F, Comment = F, CreatedTime = F, ErrorCount = F, ExpirationDate = F, FullNameLFM = F, HasErrors = F, InstitutionID = F, InstitutionName = F, IssueDate = F, LineNumber = F, ModifiedTime = F, NameID = F, StateDisplayName = F, StateID = F, TempCertificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DemographicsTempCertificationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "TempCertification", objectId = DemographicsTempCertificationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DemographicsTempCertification
	#'
	#' This function deletes a DemographicsTempCertification
	#' @param DemographicsTempCertificationID The ID of the DemographicsTempCertification to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The DemographicsTempCertificationID of the deleted DemographicsTempCertification.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDemographicsTempCertification <- function(DemographicsTempCertificationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "TempCertification", objectId = DemographicsTempCertificationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DemographicsTempCertification
	#'
	#' This function creates a DemographicsTempCertification
	#' @param fieldNames The field values to give the created DemographicsTempCertification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created DemographicsTempCertification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDemographicsTempCertification <- function(CertificationID = NULL, CertificationNumber = NULL, CertificationTypeCode = NULL, CertificationTypeCodeDescription = NULL, CertificationTypeID = NULL, Comment = NULL, ErrorCount = NULL, ExpirationDate = NULL, FullNameLFM = NULL, HasErrors = NULL, InstitutionID = NULL, InstitutionName = NULL, IssueDate = NULL, LineNumber = NULL, NameID = NULL, StateDisplayName = NULL, StateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "TempCertification", body = list(DataObject = body), searchFields = append("TempCertificationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DemographicsTempCertification
	#'
	#' This function modifies a DemographicsTempCertification
	#' @param fieldNames The field values to give the modified DemographicsTempCertification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified DemographicsTempCertification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDemographicsTempCertification <- function(TempCertificationID, CertificationID = NULL, CertificationNumber = NULL, CertificationTypeCode = NULL, CertificationTypeCodeDescription = NULL, CertificationTypeID = NULL, Comment = NULL, ErrorCount = NULL, ExpirationDate = NULL, FullNameLFM = NULL, HasErrors = NULL, InstitutionID = NULL, InstitutionName = NULL, IssueDate = NULL, LineNumber = NULL, NameID = NULL, StateDisplayName = NULL, StateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "TempCertification", objectId = TempCertificationID, body = list(DataObject = body), searchFields = append("TempCertificationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCertificationErrors
	#'
	#' This function returns a dataframe or json object of TempCertificationErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCertificationErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCertificationErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCertificationError') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of TempCertificationErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCertificationErrors <- function(searchConditionsList = NULL, CertificationID = F, CreatedTime = F, Error = F, ErrorDetail = F, LineNumber = F, ModifiedTime = F, NameLFM = F, TempCertificationErrorID = F, TempCertificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "TempCertificationError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCertificationError
	#'
	#' This function returns a dataframe or json object of a TempCertificationError
	#' @param TempCertificationErrorID The ID of the TempCertificationError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCertificationError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCertificationError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCertificationError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of TempCertificationError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCertificationError <- function(TempCertificationErrorID, CertificationID = F, CreatedTime = F, Error = F, ErrorDetail = F, LineNumber = F, ModifiedTime = F, NameLFM = F, TempCertificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCertificationErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "TempCertificationError", objectId = TempCertificationErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCertificationError
	#'
	#' This function deletes a TempCertificationError
	#' @param TempCertificationErrorID The ID of the TempCertificationError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The TempCertificationErrorID of the deleted TempCertificationError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCertificationError <- function(TempCertificationErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "TempCertificationError", objectId = TempCertificationErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCertificationError
	#'
	#' This function creates a TempCertificationError
	#' @param fieldNames The field values to give the created TempCertificationError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created TempCertificationError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCertificationError <- function(CertificationID = NULL, Error = NULL, ErrorDetail = NULL, LineNumber = NULL, NameLFM = NULL, TempCertificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "TempCertificationError", body = list(DataObject = body), searchFields = append("TempCertificationErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCertificationError
	#'
	#' This function modifies a TempCertificationError
	#' @param fieldNames The field values to give the modified TempCertificationError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified TempCertificationError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCertificationError <- function(TempCertificationErrorID, CertificationID = NULL, Error = NULL, ErrorDetail = NULL, LineNumber = NULL, NameLFM = NULL, TempCertificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "TempCertificationError", objectId = TempCertificationErrorID, body = list(DataObject = body), searchFields = append("TempCertificationErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCertificationDetails
	#'
	#' This function returns a dataframe or json object of TempCertificationDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCertificationDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCertificationDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCertificationDetail') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of TempCertificationDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCertificationDetails <- function(searchConditionsList = NULL, CertificationCompetencyCode = F, CertificationCompetencyID = F, CertificationGradeHighCodeDescription = F, CertificationGradeIDHigh = F, CertificationGradeIDLow = F, CertificationGradeLowCodeDescription = F, CertificationID = F, CertificationLevelCode = F, CertificationLevelID = F, CertificationSubjectCode = F, CertificationSubjectID = F, CreatedTime = F, ExpirationDate = F, IsHighlyQualified = F, IssueDate = F, LineNumber = F, ModifiedTime = F, TempCertificationDetailID = F, TempCertificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "TempCertificationDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCertificationDetail
	#'
	#' This function returns a dataframe or json object of a TempCertificationDetail
	#' @param TempCertificationDetailID The ID of the TempCertificationDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCertificationDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCertificationDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCertificationDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of TempCertificationDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCertificationDetail <- function(TempCertificationDetailID, CertificationCompetencyCode = F, CertificationCompetencyID = F, CertificationGradeHighCodeDescription = F, CertificationGradeIDHigh = F, CertificationGradeIDLow = F, CertificationGradeLowCodeDescription = F, CertificationID = F, CertificationLevelCode = F, CertificationLevelID = F, CertificationSubjectCode = F, CertificationSubjectID = F, CreatedTime = F, ExpirationDate = F, IsHighlyQualified = F, IssueDate = F, LineNumber = F, ModifiedTime = F, TempCertificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCertificationDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "TempCertificationDetail", objectId = TempCertificationDetailID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCertificationDetail
	#'
	#' This function deletes a TempCertificationDetail
	#' @param TempCertificationDetailID The ID of the TempCertificationDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The TempCertificationDetailID of the deleted TempCertificationDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCertificationDetail <- function(TempCertificationDetailID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "TempCertificationDetail", objectId = TempCertificationDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCertificationDetail
	#'
	#' This function creates a TempCertificationDetail
	#' @param fieldNames The field values to give the created TempCertificationDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created TempCertificationDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCertificationDetail <- function(CertificationCompetencyCode = NULL, CertificationCompetencyID = NULL, CertificationGradeHighCodeDescription = NULL, CertificationGradeIDHigh = NULL, CertificationGradeIDLow = NULL, CertificationGradeLowCodeDescription = NULL, CertificationID = NULL, CertificationLevelCode = NULL, CertificationLevelID = NULL, CertificationSubjectCode = NULL, CertificationSubjectID = NULL, ExpirationDate = NULL, IsHighlyQualified = NULL, IssueDate = NULL, LineNumber = NULL, TempCertificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "TempCertificationDetail", body = list(DataObject = body), searchFields = append("TempCertificationDetailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCertificationDetail
	#'
	#' This function modifies a TempCertificationDetail
	#' @param fieldNames The field values to give the modified TempCertificationDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified TempCertificationDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCertificationDetail <- function(TempCertificationDetailID, CertificationCompetencyCode = NULL, CertificationCompetencyID = NULL, CertificationGradeHighCodeDescription = NULL, CertificationGradeIDHigh = NULL, CertificationGradeIDLow = NULL, CertificationGradeLowCodeDescription = NULL, CertificationID = NULL, CertificationLevelCode = NULL, CertificationLevelID = NULL, CertificationSubjectCode = NULL, CertificationSubjectID = NULL, ExpirationDate = NULL, IsHighlyQualified = NULL, IssueDate = NULL, LineNumber = NULL, TempCertificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "TempCertificationDetail", objectId = TempCertificationDetailID, body = list(DataObject = body), searchFields = append("TempCertificationDetailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DemographicsTempExceptions
	#'
	#' This function returns a dataframe or json object of DemographicsTempExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DemographicsTempExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DemographicsTempExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DemographicsTempException') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of DemographicsTempExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDemographicsTempExceptions <- function(searchConditionsList = NULL, CreatedTime = F, LineNumber = F, Message = F, ModifiedTime = F, NameLFM = F, TempExceptionID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "TempException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DemographicsTempException
	#'
	#' This function returns a dataframe or json object of a DemographicsTempException
	#' @param DemographicsTempExceptionID The ID of the DemographicsTempException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DemographicsTempException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DemographicsTempException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DemographicsTempException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of DemographicsTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDemographicsTempException <- function(DemographicsTempExceptionID, CreatedTime = F, LineNumber = F, Message = F, ModifiedTime = F, NameLFM = F, TempExceptionID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DemographicsTempExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "TempException", objectId = DemographicsTempExceptionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DemographicsTempException
	#'
	#' This function deletes a DemographicsTempException
	#' @param DemographicsTempExceptionID The ID of the DemographicsTempException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The DemographicsTempExceptionID of the deleted DemographicsTempException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDemographicsTempException <- function(DemographicsTempExceptionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "TempException", objectId = DemographicsTempExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DemographicsTempException
	#'
	#' This function creates a DemographicsTempException
	#' @param fieldNames The field values to give the created DemographicsTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created DemographicsTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDemographicsTempException <- function(LineNumber = NULL, Message = NULL, NameLFM = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "TempException", body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DemographicsTempException
	#'
	#' This function modifies a DemographicsTempException
	#' @param fieldNames The field values to give the modified DemographicsTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified DemographicsTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDemographicsTempException <- function(TempExceptionID, LineNumber = NULL, Message = NULL, NameLFM = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "TempException", objectId = TempExceptionID, body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NameMergeRunHistories
	#'
	#' This function returns a dataframe or json object of NameMergeRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameMergeRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameMergeRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameMergeRunHistory') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of NameMergeRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNameMergeRunHistories <- function(searchConditionsList = NULL, BirthDateFrom = F, BirthDateTo = F, CreatedTime = F, FullNameLFMFrom = F, FullNameLFMTo = F, ModifiedTime = F, NameIDFrom = F, NameIDTo = F, NameMergeRunHistoryID = F, NameUsedByFrom = F, NameUsedByTo = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "NameMergeRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NameMergeRunHistory
	#'
	#' This function returns a dataframe or json object of a NameMergeRunHistory
	#' @param NameMergeRunHistoryID The ID of the NameMergeRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameMergeRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameMergeRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameMergeRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of NameMergeRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNameMergeRunHistory <- function(NameMergeRunHistoryID, BirthDateFrom = F, BirthDateTo = F, CreatedTime = F, FullNameLFMFrom = F, FullNameLFMTo = F, ModifiedTime = F, NameIDFrom = F, NameIDTo = F, NameUsedByFrom = F, NameUsedByTo = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NameMergeRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "NameMergeRunHistory", objectId = NameMergeRunHistoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NameMergeRunHistory
	#'
	#' This function deletes a NameMergeRunHistory
	#' @param NameMergeRunHistoryID The ID of the NameMergeRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The NameMergeRunHistoryID of the deleted NameMergeRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNameMergeRunHistory <- function(NameMergeRunHistoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "NameMergeRunHistory", objectId = NameMergeRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NameMergeRunHistory
	#'
	#' This function creates a NameMergeRunHistory
	#' @param fieldNames The field values to give the created NameMergeRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created NameMergeRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNameMergeRunHistory <- function(BirthDateFrom = NULL, BirthDateTo = NULL, FullNameLFMFrom = NULL, FullNameLFMTo = NULL, NameIDFrom = NULL, NameIDTo = NULL, NameUsedByFrom = NULL, NameUsedByTo = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "NameMergeRunHistory", body = list(DataObject = body), searchFields = append("NameMergeRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NameMergeRunHistory
	#'
	#' This function modifies a NameMergeRunHistory
	#' @param fieldNames The field values to give the modified NameMergeRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified NameMergeRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNameMergeRunHistory <- function(NameMergeRunHistoryID, BirthDateFrom = NULL, BirthDateTo = NULL, FullNameLFMFrom = NULL, FullNameLFMTo = NULL, NameIDFrom = NULL, NameIDTo = NULL, NameUsedByFrom = NULL, NameUsedByTo = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "NameMergeRunHistory", objectId = NameMergeRunHistoryID, body = list(DataObject = body), searchFields = append("NameMergeRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Occupations
	#'
	#' This function returns a dataframe or json object of Occupations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Occupations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Occupations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Occupation') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of Occupations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOccupations <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, OccupationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "Occupation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Occupation
	#'
	#' This function returns a dataframe or json object of an Occupation
	#' @param OccupationID The ID of the Occupation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Occupation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Occupation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Occupation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of Occupation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOccupation <- function(OccupationID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OccupationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "Occupation", objectId = OccupationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Occupation
	#'
	#' This function deletes an Occupation
	#' @param OccupationID The ID of the Occupation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The OccupationID of the deleted Occupation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOccupation <- function(OccupationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "Occupation", objectId = OccupationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Occupation
	#'
	#' This function creates an Occupation
	#' @param fieldNames The field values to give the created Occupation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created Occupation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOccupation <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "Occupation", body = list(DataObject = body), searchFields = append("OccupationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Occupation
	#'
	#' This function modifies an Occupation
	#' @param fieldNames The field values to give the modified Occupation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified Occupation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOccupation <- function(OccupationID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "Occupation", objectId = OccupationID, body = list(DataObject = body), searchFields = append("OccupationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DemographicsConfigDistricts
	#'
	#' This function returns a dataframe or json object of DemographicsConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DemographicsConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DemographicsConfigDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DemographicsConfigDistrict') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of DemographicsConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDemographicsConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, CreatedTime = F, DistrictID = F, EnforceAddressRangeDefaults = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DemographicsConfigDistrict
	#'
	#' This function returns a dataframe or json object of a DemographicsConfigDistrict
	#' @param DemographicsConfigDistrictID The ID of the DemographicsConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DemographicsConfigDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DemographicsConfigDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DemographicsConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of DemographicsConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDemographicsConfigDistrict <- function(DemographicsConfigDistrictID, ConfigDistrictID = F, CreatedTime = F, DistrictID = F, EnforceAddressRangeDefaults = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DemographicsConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "ConfigDistrict", objectId = DemographicsConfigDistrictID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DemographicsConfigDistrict
	#'
	#' This function deletes a DemographicsConfigDistrict
	#' @param DemographicsConfigDistrictID The ID of the DemographicsConfigDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The DemographicsConfigDistrictID of the deleted DemographicsConfigDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDemographicsConfigDistrict <- function(DemographicsConfigDistrictID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "ConfigDistrict", objectId = DemographicsConfigDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DemographicsConfigDistrict
	#'
	#' This function creates a DemographicsConfigDistrict
	#' @param fieldNames The field values to give the created DemographicsConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created DemographicsConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDemographicsConfigDistrict <- function(DistrictID = NULL, EnforceAddressRangeDefaults = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "ConfigDistrict", body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DemographicsConfigDistrict
	#'
	#' This function modifies a DemographicsConfigDistrict
	#' @param fieldNames The field values to give the modified DemographicsConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified DemographicsConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDemographicsConfigDistrict <- function(ConfigDistrictID, DistrictID = NULL, EnforceAddressRangeDefaults = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "ConfigDistrict", objectId = ConfigDistrictID, body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationThirdPartyFormatCertificationGrades
	#'
	#' This function returns a dataframe or json object of CertificationThirdPartyFormatCertificationGrades
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyFormatCertificationGrades. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyFormatCertificationGrades.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyFormatCertificationGrade') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationThirdPartyFormatCertificationGrades
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationThirdPartyFormatCertificationGrades <- function(searchConditionsList = NULL, CertificationGradeID = F, CertificationThirdPartyFormatCertificationGradeID = F, CertificationThirdPartyFormatID = F, CreatedTime = F, ImportValue = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationGrade", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationThirdPartyFormatCertificationGrade
	#'
	#' This function returns a dataframe or json object of a CertificationThirdPartyFormatCertificationGrade
	#' @param CertificationThirdPartyFormatCertificationGradeID The ID of the CertificationThirdPartyFormatCertificationGrade to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationThirdPartyFormatCertificationGrade. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationThirdPartyFormatCertificationGrade.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationThirdPartyFormatCertificationGrade') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationThirdPartyFormatCertificationGrade
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationThirdPartyFormatCertificationGrade <- function(CertificationThirdPartyFormatCertificationGradeID, CertificationGradeID = F, CertificationThirdPartyFormatID = F, CreatedTime = F, ImportValue = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationThirdPartyFormatCertificationGradeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationGrade", objectId = CertificationThirdPartyFormatCertificationGradeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationThirdPartyFormatCertificationGrade
	#'
	#' This function deletes a CertificationThirdPartyFormatCertificationGrade
	#' @param CertificationThirdPartyFormatCertificationGradeID The ID of the CertificationThirdPartyFormatCertificationGrade to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationThirdPartyFormatCertificationGradeID of the deleted CertificationThirdPartyFormatCertificationGrade.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationThirdPartyFormatCertificationGrade <- function(CertificationThirdPartyFormatCertificationGradeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationGrade", objectId = CertificationThirdPartyFormatCertificationGradeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationThirdPartyFormatCertificationGrade
	#'
	#' This function creates a CertificationThirdPartyFormatCertificationGrade
	#' @param fieldNames The field values to give the created CertificationThirdPartyFormatCertificationGrade. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationThirdPartyFormatCertificationGrade
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationThirdPartyFormatCertificationGrade <- function(CertificationGradeID = NULL, CertificationThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationGrade", body = list(DataObject = body), searchFields = append("CertificationThirdPartyFormatCertificationGradeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationThirdPartyFormatCertificationGrade
	#'
	#' This function modifies a CertificationThirdPartyFormatCertificationGrade
	#' @param fieldNames The field values to give the modified CertificationThirdPartyFormatCertificationGrade. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationThirdPartyFormatCertificationGrade
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationThirdPartyFormatCertificationGrade <- function(CertificationThirdPartyFormatCertificationGradeID, CertificationGradeID = NULL, CertificationThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationThirdPartyFormatCertificationGrade", objectId = CertificationThirdPartyFormatCertificationGradeID, body = list(DataObject = body), searchFields = append("CertificationThirdPartyFormatCertificationGradeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CertificationGrades
	#'
	#' This function returns a dataframe or json object of CertificationGrades
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationGrades. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationGrades.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationGrade') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of CertificationGrades
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCertificationGrades <- function(searchConditionsList = NULL, CertificationGradeID = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "CertificationGrade", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CertificationGrade
	#'
	#' This function returns a dataframe or json object of a CertificationGrade
	#' @param CertificationGradeID The ID of the CertificationGrade to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CertificationGrade. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CertificationGrade.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CertificationGrade') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of CertificationGrade
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCertificationGrade <- function(CertificationGradeID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CertificationGradeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "CertificationGrade", objectId = CertificationGradeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CertificationGrade
	#'
	#' This function deletes a CertificationGrade
	#' @param CertificationGradeID The ID of the CertificationGrade to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CertificationGradeID of the deleted CertificationGrade.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCertificationGrade <- function(CertificationGradeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "CertificationGrade", objectId = CertificationGradeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CertificationGrade
	#'
	#' This function creates a CertificationGrade
	#' @param fieldNames The field values to give the created CertificationGrade. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created CertificationGrade
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCertificationGrade <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "CertificationGrade", body = list(DataObject = body), searchFields = append("CertificationGradeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CertificationGrade
	#'
	#' This function modifies a CertificationGrade
	#' @param fieldNames The field values to give the modified CertificationGrade. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified CertificationGrade
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCertificationGrade <- function(CertificationGradeID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "CertificationGrade", objectId = CertificationGradeID, body = list(DataObject = body), searchFields = append("CertificationGradeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DemographicsTempEmergencyContacts
	#'
	#' This function returns a dataframe or json object of DemographicsTempEmergencyContacts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DemographicsTempEmergencyContacts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DemographicsTempEmergencyContacts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DemographicsTempEmergencyContact') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of DemographicsTempEmergencyContacts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDemographicsTempEmergencyContacts <- function(searchConditionsList = NULL, AllowPickUp = F, CreatedTime = F, EmergencyContactFor = F, EmergencyContactName = F, ExceptionNote = F, HasActiveRestrictedAccess = F, HasExceptions = F, ModifiedTime = F, NameID = F, NameIDEmergencyContact = F, Rank = F, RelationshipDescription = F, RelationshipID = F, TempEmergencyContactID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "TempEmergencyContact", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DemographicsTempEmergencyContact
	#'
	#' This function returns a dataframe or json object of a DemographicsTempEmergencyContact
	#' @param DemographicsTempEmergencyContactID The ID of the DemographicsTempEmergencyContact to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DemographicsTempEmergencyContact. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DemographicsTempEmergencyContact.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DemographicsTempEmergencyContact') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of DemographicsTempEmergencyContact
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDemographicsTempEmergencyContact <- function(DemographicsTempEmergencyContactID, AllowPickUp = F, CreatedTime = F, EmergencyContactFor = F, EmergencyContactName = F, ExceptionNote = F, HasActiveRestrictedAccess = F, HasExceptions = F, ModifiedTime = F, NameID = F, NameIDEmergencyContact = F, Rank = F, RelationshipDescription = F, RelationshipID = F, TempEmergencyContactID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DemographicsTempEmergencyContactID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "TempEmergencyContact", objectId = DemographicsTempEmergencyContactID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DemographicsTempEmergencyContact
	#'
	#' This function deletes a DemographicsTempEmergencyContact
	#' @param DemographicsTempEmergencyContactID The ID of the DemographicsTempEmergencyContact to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The DemographicsTempEmergencyContactID of the deleted DemographicsTempEmergencyContact.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDemographicsTempEmergencyContact <- function(DemographicsTempEmergencyContactID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "TempEmergencyContact", objectId = DemographicsTempEmergencyContactID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DemographicsTempEmergencyContact
	#'
	#' This function creates a DemographicsTempEmergencyContact
	#' @param fieldNames The field values to give the created DemographicsTempEmergencyContact. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created DemographicsTempEmergencyContact
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDemographicsTempEmergencyContact <- function(AllowPickUp = NULL, EmergencyContactFor = NULL, EmergencyContactName = NULL, ExceptionNote = NULL, HasActiveRestrictedAccess = NULL, HasExceptions = NULL, NameID = NULL, NameIDEmergencyContact = NULL, Rank = NULL, RelationshipDescription = NULL, RelationshipID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "TempEmergencyContact", body = list(DataObject = body), searchFields = append("TempEmergencyContactID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DemographicsTempEmergencyContact
	#'
	#' This function modifies a DemographicsTempEmergencyContact
	#' @param fieldNames The field values to give the modified DemographicsTempEmergencyContact. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified DemographicsTempEmergencyContact
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDemographicsTempEmergencyContact <- function(TempEmergencyContactID, AllowPickUp = NULL, EmergencyContactFor = NULL, EmergencyContactName = NULL, ExceptionNote = NULL, HasActiveRestrictedAccess = NULL, HasExceptions = NULL, NameID = NULL, NameIDEmergencyContact = NULL, Rank = NULL, RelationshipDescription = NULL, RelationshipID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "TempEmergencyContact", objectId = TempEmergencyContactID, body = list(DataObject = body), searchFields = append("TempEmergencyContactID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List LastNameNumbers
	#'
	#' This function returns a dataframe or json object of LastNameNumbers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LastNameNumbers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LastNameNumbers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LastNameNumber') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of LastNameNumbers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLastNameNumbers <- function(searchConditionsList = NULL, ConfigSystemID = F, CreatedTime = F, LastNameNumberID = F, ModifiedTime = F, NameNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "LastNameNumber", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a LastNameNumber
	#'
	#' This function returns a dataframe or json object of a LastNameNumber
	#' @param LastNameNumberID The ID of the LastNameNumber to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LastNameNumber. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LastNameNumber.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LastNameNumber') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of LastNameNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLastNameNumber <- function(LastNameNumberID, ConfigSystemID = F, CreatedTime = F, ModifiedTime = F, NameNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LastNameNumberID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "LastNameNumber", objectId = LastNameNumberID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a LastNameNumber
	#'
	#' This function deletes a LastNameNumber
	#' @param LastNameNumberID The ID of the LastNameNumber to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The LastNameNumberID of the deleted LastNameNumber.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLastNameNumber <- function(LastNameNumberID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "LastNameNumber", objectId = LastNameNumberID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a LastNameNumber
	#'
	#' This function creates a LastNameNumber
	#' @param fieldNames The field values to give the created LastNameNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created LastNameNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLastNameNumber <- function(ConfigSystemID = NULL, NameNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "LastNameNumber", body = list(DataObject = body), searchFields = append("LastNameNumberID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a LastNameNumber
	#'
	#' This function modifies a LastNameNumber
	#' @param fieldNames The field values to give the modified LastNameNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified LastNameNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLastNameNumber <- function(LastNameNumberID, ConfigSystemID = NULL, NameNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "LastNameNumber", objectId = LastNameNumberID, body = list(DataObject = body), searchFields = append("LastNameNumberID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempNameNumbers
	#'
	#' This function returns a dataframe or json object of TempNameNumbers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNameNumbers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNameNumbers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNameNumber') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of TempNameNumbers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempNameNumbers <- function(searchConditionsList = NULL, ConflictReason = F, CreatedTime = F, EmployeeID = F, FullNameLFM = F, HasConflicts = F, ModifiedTime = F, NameID = F, NameNumber = F, NewEmployeeNumber = F, NewVendorNumber = F, OldEmployeeNumber = F, OldVendorNumber = F, TempNameNumberID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, VendorID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "TempNameNumber", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempNameNumber
	#'
	#' This function returns a dataframe or json object of a TempNameNumber
	#' @param TempNameNumberID The ID of the TempNameNumber to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNameNumber. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNameNumber.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNameNumber') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of TempNameNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempNameNumber <- function(TempNameNumberID, ConflictReason = F, CreatedTime = F, EmployeeID = F, FullNameLFM = F, HasConflicts = F, ModifiedTime = F, NameID = F, NameNumber = F, NewEmployeeNumber = F, NewVendorNumber = F, OldEmployeeNumber = F, OldVendorNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, VendorID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempNameNumberID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "TempNameNumber", objectId = TempNameNumberID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempNameNumber
	#'
	#' This function deletes a TempNameNumber
	#' @param TempNameNumberID The ID of the TempNameNumber to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The TempNameNumberID of the deleted TempNameNumber.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempNameNumber <- function(TempNameNumberID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "TempNameNumber", objectId = TempNameNumberID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempNameNumber
	#'
	#' This function creates a TempNameNumber
	#' @param fieldNames The field values to give the created TempNameNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created TempNameNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempNameNumber <- function(ConflictReason = NULL, EmployeeID = NULL, FullNameLFM = NULL, HasConflicts = NULL, NameID = NULL, NameNumber = NULL, NewEmployeeNumber = NULL, NewVendorNumber = NULL, OldEmployeeNumber = NULL, OldVendorNumber = NULL, VendorID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "TempNameNumber", body = list(DataObject = body), searchFields = append("TempNameNumberID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempNameNumber
	#'
	#' This function modifies a TempNameNumber
	#' @param fieldNames The field values to give the modified TempNameNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified TempNameNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempNameNumber <- function(TempNameNumberID, ConflictReason = NULL, EmployeeID = NULL, FullNameLFM = NULL, HasConflicts = NULL, NameID = NULL, NameNumber = NULL, NewEmployeeNumber = NULL, NewVendorNumber = NULL, OldEmployeeNumber = NULL, OldVendorNumber = NULL, VendorID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "TempNameNumber", objectId = TempNameNumberID, body = list(DataObject = body), searchFields = append("TempNameNumberID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NameEthnicityRaceSubcategoryMNS
	#'
	#' This function returns a dataframe or json object of NameEthnicityRaceSubcategoryMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameEthnicityRaceSubcategoryMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameEthnicityRaceSubcategoryMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameEthnicityRaceSubcategoryMN') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of NameEthnicityRaceSubcategoryMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNameEthnicityRaceSubcategoryMNS <- function(searchConditionsList = NULL, CreatedTime = F, EthnicityRaceType = F, ModifiedTime = F, NameEthnicityRaceSubcategoryMNID = F, NameID = F, StateEthnicityRaceSubcategoryMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "NameEthnicityRaceSubcategoryMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NameEthnicityRaceSubcategoryMN
	#'
	#' This function returns a dataframe or json object of a NameEthnicityRaceSubcategoryMN
	#' @param NameEthnicityRaceSubcategoryMNID The ID of the NameEthnicityRaceSubcategoryMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NameEthnicityRaceSubcategoryMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameEthnicityRaceSubcategoryMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameEthnicityRaceSubcategoryMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of NameEthnicityRaceSubcategoryMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNameEthnicityRaceSubcategoryMN <- function(NameEthnicityRaceSubcategoryMNID, CreatedTime = F, EthnicityRaceType = F, ModifiedTime = F, NameID = F, StateEthnicityRaceSubcategoryMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NameEthnicityRaceSubcategoryMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "NameEthnicityRaceSubcategoryMN", objectId = NameEthnicityRaceSubcategoryMNID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NameEthnicityRaceSubcategoryMN
	#'
	#' This function deletes a NameEthnicityRaceSubcategoryMN
	#' @param NameEthnicityRaceSubcategoryMNID The ID of the NameEthnicityRaceSubcategoryMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The NameEthnicityRaceSubcategoryMNID of the deleted NameEthnicityRaceSubcategoryMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNameEthnicityRaceSubcategoryMN <- function(NameEthnicityRaceSubcategoryMNID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "NameEthnicityRaceSubcategoryMN", objectId = NameEthnicityRaceSubcategoryMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NameEthnicityRaceSubcategoryMN
	#'
	#' This function creates a NameEthnicityRaceSubcategoryMN
	#' @param fieldNames The field values to give the created NameEthnicityRaceSubcategoryMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created NameEthnicityRaceSubcategoryMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNameEthnicityRaceSubcategoryMN <- function(EthnicityRaceType = NULL, NameID = NULL, StateEthnicityRaceSubcategoryMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "NameEthnicityRaceSubcategoryMN", body = list(DataObject = body), searchFields = append("NameEthnicityRaceSubcategoryMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NameEthnicityRaceSubcategoryMN
	#'
	#' This function modifies a NameEthnicityRaceSubcategoryMN
	#' @param fieldNames The field values to give the modified NameEthnicityRaceSubcategoryMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified NameEthnicityRaceSubcategoryMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNameEthnicityRaceSubcategoryMN <- function(NameEthnicityRaceSubcategoryMNID, EthnicityRaceType = NULL, NameID = NULL, StateEthnicityRaceSubcategoryMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "NameEthnicityRaceSubcategoryMN", objectId = NameEthnicityRaceSubcategoryMNID, body = list(DataObject = body), searchFields = append("NameEthnicityRaceSubcategoryMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempNameEmails
	#'
	#' This function returns a dataframe or json object of TempNameEmails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNameEmails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNameEmails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNameEmail') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of TempNameEmails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempNameEmails <- function(searchConditionsList = NULL, CreatedTime = F, EmailAddress = F, EmailTypeID = F, ErrorCount = F, FullNameFML = F, HasError = F, ModifiedTime = F, NameEmailID = F, NameID = F, Note = F, Rank = F, TempNameEmailID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "TempNameEmail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempNameEmail
	#'
	#' This function returns a dataframe or json object of a TempNameEmail
	#' @param TempNameEmailID The ID of the TempNameEmail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNameEmail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNameEmail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNameEmail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of TempNameEmail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempNameEmail <- function(TempNameEmailID, CreatedTime = F, EmailAddress = F, EmailTypeID = F, ErrorCount = F, FullNameFML = F, HasError = F, ModifiedTime = F, NameEmailID = F, NameID = F, Note = F, Rank = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempNameEmailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "TempNameEmail", objectId = TempNameEmailID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempNameEmail
	#'
	#' This function deletes a TempNameEmail
	#' @param TempNameEmailID The ID of the TempNameEmail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The TempNameEmailID of the deleted TempNameEmail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempNameEmail <- function(TempNameEmailID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "TempNameEmail", objectId = TempNameEmailID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempNameEmail
	#'
	#' This function creates a TempNameEmail
	#' @param fieldNames The field values to give the created TempNameEmail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created TempNameEmail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempNameEmail <- function(EmailAddress = NULL, EmailTypeID = NULL, ErrorCount = NULL, FullNameFML = NULL, HasError = NULL, NameEmailID = NULL, NameID = NULL, Note = NULL, Rank = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "TempNameEmail", body = list(DataObject = body), searchFields = append("TempNameEmailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempNameEmail
	#'
	#' This function modifies a TempNameEmail
	#' @param fieldNames The field values to give the modified TempNameEmail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified TempNameEmail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempNameEmail <- function(TempNameEmailID, EmailAddress = NULL, EmailTypeID = NULL, ErrorCount = NULL, FullNameFML = NULL, HasError = NULL, NameEmailID = NULL, NameID = NULL, Note = NULL, Rank = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "TempNameEmail", objectId = TempNameEmailID, body = list(DataObject = body), searchFields = append("TempNameEmailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempNameEmailErrors
	#'
	#' This function returns a dataframe or json object of TempNameEmailErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNameEmailErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNameEmailErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNameEmailError') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of TempNameEmailErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempNameEmailErrors <- function(searchConditionsList = NULL, CreatedTime = F, ErrorField = F, ErrorNumber = F, ErrorText = F, ModifiedTime = F, TempNameEmailErrorID = F, TempNameEmailID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "TempNameEmailError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempNameEmailError
	#'
	#' This function returns a dataframe or json object of a TempNameEmailError
	#' @param TempNameEmailErrorID The ID of the TempNameEmailError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNameEmailError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNameEmailError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNameEmailError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of TempNameEmailError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempNameEmailError <- function(TempNameEmailErrorID, CreatedTime = F, ErrorField = F, ErrorNumber = F, ErrorText = F, ModifiedTime = F, TempNameEmailID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempNameEmailErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "TempNameEmailError", objectId = TempNameEmailErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempNameEmailError
	#'
	#' This function deletes a TempNameEmailError
	#' @param TempNameEmailErrorID The ID of the TempNameEmailError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The TempNameEmailErrorID of the deleted TempNameEmailError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempNameEmailError <- function(TempNameEmailErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "TempNameEmailError", objectId = TempNameEmailErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempNameEmailError
	#'
	#' This function creates a TempNameEmailError
	#' @param fieldNames The field values to give the created TempNameEmailError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created TempNameEmailError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempNameEmailError <- function(ErrorField = NULL, ErrorNumber = NULL, ErrorText = NULL, TempNameEmailID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "TempNameEmailError", body = list(DataObject = body), searchFields = append("TempNameEmailErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempNameEmailError
	#'
	#' This function modifies a TempNameEmailError
	#' @param fieldNames The field values to give the modified TempNameEmailError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified TempNameEmailError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempNameEmailError <- function(TempNameEmailErrorID, ErrorField = NULL, ErrorNumber = NULL, ErrorText = NULL, TempNameEmailID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "TempNameEmailError", objectId = TempNameEmailErrorID, body = list(DataObject = body), searchFields = append("TempNameEmailErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List RaisersEdgeObjectMAS
	#'
	#' This function returns a dataframe or json object of RaisersEdgeObjectMAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RaisersEdgeObjectMAS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RaisersEdgeObjectMAS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RaisersEdgeObjectMA') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of RaisersEdgeObjectMAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRaisersEdgeObjectMAS <- function(searchConditionsList = NULL, NameID = F, RaisersEdgeID = F, RaisersEdgeObjectMAID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "RaisersEdgeObjectMA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RaisersEdgeObjectMA
	#'
	#' This function returns a dataframe or json object of a RaisersEdgeObjectMA
	#' @param RaisersEdgeObjectMAID The ID of the RaisersEdgeObjectMA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RaisersEdgeObjectMA. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RaisersEdgeObjectMA.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RaisersEdgeObjectMA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of RaisersEdgeObjectMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRaisersEdgeObjectMA <- function(RaisersEdgeObjectMAID, NameID = F, RaisersEdgeID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RaisersEdgeObjectMAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "RaisersEdgeObjectMA", objectId = RaisersEdgeObjectMAID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RaisersEdgeObjectMA
	#'
	#' This function deletes a RaisersEdgeObjectMA
	#' @param RaisersEdgeObjectMAID The ID of the RaisersEdgeObjectMA to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The RaisersEdgeObjectMAID of the deleted RaisersEdgeObjectMA.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRaisersEdgeObjectMA <- function(RaisersEdgeObjectMAID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "RaisersEdgeObjectMA", objectId = RaisersEdgeObjectMAID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RaisersEdgeObjectMA
	#'
	#' This function creates a RaisersEdgeObjectMA
	#' @param fieldNames The field values to give the created RaisersEdgeObjectMA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created RaisersEdgeObjectMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRaisersEdgeObjectMA <- function(NameID = NULL, RaisersEdgeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "RaisersEdgeObjectMA", body = list(DataObject = body), searchFields = append("RaisersEdgeObjectMAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RaisersEdgeObjectMA
	#'
	#' This function modifies a RaisersEdgeObjectMA
	#' @param fieldNames The field values to give the modified RaisersEdgeObjectMA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified RaisersEdgeObjectMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRaisersEdgeObjectMA <- function(RaisersEdgeObjectMAID, NameID = NULL, RaisersEdgeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "RaisersEdgeObjectMA", objectId = RaisersEdgeObjectMAID, body = list(DataObject = body), searchFields = append("RaisersEdgeObjectMAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMassUpdateGeolocationAddresses
	#'
	#' This function returns a dataframe or json object of TempMassUpdateGeolocationAddresses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassUpdateGeolocationAddresses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassUpdateGeolocationAddresses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassUpdateGeolocationAddress') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of TempMassUpdateGeolocationAddresses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassUpdateGeolocationAddresses <- function(searchConditionsList = NULL, AddressID = F, ConfidenceRating = F, CreatedTime = F, CurrentGeoID = F, CurrentLatitude = F, CurrentLongitude = F, FullAddress = F, HasError = F, ModifiedTime = F, PreviousWasAPILoaded = F, TempMassUpdateGeolocationAddressID = F, UpdatedGeoID = F, UpdatedLatitude = F, UpdatedLongitude = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "TempMassUpdateGeolocationAddress", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassUpdateGeolocationAddress
	#'
	#' This function returns a dataframe or json object of a TempMassUpdateGeolocationAddress
	#' @param TempMassUpdateGeolocationAddressID The ID of the TempMassUpdateGeolocationAddress to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassUpdateGeolocationAddress. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassUpdateGeolocationAddress.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassUpdateGeolocationAddress') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of TempMassUpdateGeolocationAddress
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassUpdateGeolocationAddress <- function(TempMassUpdateGeolocationAddressID, AddressID = F, ConfidenceRating = F, CreatedTime = F, CurrentGeoID = F, CurrentLatitude = F, CurrentLongitude = F, FullAddress = F, HasError = F, ModifiedTime = F, PreviousWasAPILoaded = F, UpdatedGeoID = F, UpdatedLatitude = F, UpdatedLongitude = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassUpdateGeolocationAddressID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "TempMassUpdateGeolocationAddress", objectId = TempMassUpdateGeolocationAddressID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassUpdateGeolocationAddress
	#'
	#' This function deletes a TempMassUpdateGeolocationAddress
	#' @param TempMassUpdateGeolocationAddressID The ID of the TempMassUpdateGeolocationAddress to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The TempMassUpdateGeolocationAddressID of the deleted TempMassUpdateGeolocationAddress.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassUpdateGeolocationAddress <- function(TempMassUpdateGeolocationAddressID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "TempMassUpdateGeolocationAddress", objectId = TempMassUpdateGeolocationAddressID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassUpdateGeolocationAddress
	#'
	#' This function creates a TempMassUpdateGeolocationAddress
	#' @param fieldNames The field values to give the created TempMassUpdateGeolocationAddress. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created TempMassUpdateGeolocationAddress
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassUpdateGeolocationAddress <- function(ConfidenceRating = NULL, CurrentGeoID = NULL, CurrentLatitude = NULL, CurrentLongitude = NULL, FullAddress = NULL, HasError = NULL, PreviousWasAPILoaded = NULL, UpdatedGeoID = NULL, UpdatedLatitude = NULL, UpdatedLongitude = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "TempMassUpdateGeolocationAddress", body = list(DataObject = body), searchFields = append("TempMassUpdateGeolocationAddressID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassUpdateGeolocationAddress
	#'
	#' This function modifies a TempMassUpdateGeolocationAddress
	#' @param fieldNames The field values to give the modified TempMassUpdateGeolocationAddress. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified TempMassUpdateGeolocationAddress
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassUpdateGeolocationAddress <- function(TempMassUpdateGeolocationAddressID, ConfidenceRating = NULL, CurrentGeoID = NULL, CurrentLatitude = NULL, CurrentLongitude = NULL, FullAddress = NULL, HasError = NULL, PreviousWasAPILoaded = NULL, UpdatedGeoID = NULL, UpdatedLatitude = NULL, UpdatedLongitude = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "TempMassUpdateGeolocationAddress", objectId = TempMassUpdateGeolocationAddressID, body = list(DataObject = body), searchFields = append("TempMassUpdateGeolocationAddressID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EducationLevels
	#'
	#' This function returns a dataframe or json object of EducationLevels
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EducationLevels. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EducationLevels.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EducationLevel') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of EducationLevels
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEducationLevels <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EdFiLevelOfEducationTypeID = F, EducationLevelID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "EducationLevel", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EducationLevel
	#'
	#' This function returns a dataframe or json object of an EducationLevel
	#' @param EducationLevelID The ID of the EducationLevel to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EducationLevel. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EducationLevel.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EducationLevel') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of EducationLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEducationLevel <- function(EducationLevelID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EdFiLevelOfEducationTypeID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EducationLevelID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "EducationLevel", objectId = EducationLevelID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EducationLevel
	#'
	#' This function deletes an EducationLevel
	#' @param EducationLevelID The ID of the EducationLevel to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The EducationLevelID of the deleted EducationLevel.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEducationLevel <- function(EducationLevelID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "EducationLevel", objectId = EducationLevelID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EducationLevel
	#'
	#' This function creates an EducationLevel
	#' @param fieldNames The field values to give the created EducationLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created EducationLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEducationLevel <- function(Code = NULL, Description = NULL, EdFiLevelOfEducationTypeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "EducationLevel", body = list(DataObject = body), searchFields = append("EducationLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EducationLevel
	#'
	#' This function modifies an EducationLevel
	#' @param fieldNames The field values to give the modified EducationLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified EducationLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEducationLevel <- function(EducationLevelID, Code = NULL, Description = NULL, EdFiLevelOfEducationTypeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "EducationLevel", objectId = EducationLevelID, body = list(DataObject = body), searchFields = append("EducationLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Countries
	#'
	#' This function returns a dataframe or json object of Countries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Countries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Countries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Country') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of Countries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCountries <- function(searchConditionsList = NULL, CountryID = F, CreatedTime = F, EdFiCountryID = F, ModifiedTime = F, Name = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "Country", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Country
	#'
	#' This function returns a dataframe or json object of a Country
	#' @param CountryID The ID of the Country to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Country. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Country.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Country') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of Country
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCountry <- function(CountryID, CreatedTime = F, EdFiCountryID = F, ModifiedTime = F, Name = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CountryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "Country", objectId = CountryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Country
	#'
	#' This function deletes a Country
	#' @param CountryID The ID of the Country to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The CountryID of the deleted Country.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCountry <- function(CountryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "Country", objectId = CountryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Country
	#'
	#' This function creates a Country
	#' @param fieldNames The field values to give the created Country. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created Country
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCountry <- function(EdFiCountryID = NULL, Name = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "Country", body = list(DataObject = body), searchFields = append("CountryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Country
	#'
	#' This function modifies a Country
	#' @param fieldNames The field values to give the modified Country. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified Country
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCountry <- function(CountryID, EdFiCountryID = NULL, Name = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "Country", objectId = CountryID, body = list(DataObject = body), searchFields = append("CountryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMassUpdateGeolocationAddressErrors
	#'
	#' This function returns a dataframe or json object of TempMassUpdateGeolocationAddressErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassUpdateGeolocationAddressErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassUpdateGeolocationAddressErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassUpdateGeolocationAddressError') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of TempMassUpdateGeolocationAddressErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassUpdateGeolocationAddressErrors <- function(searchConditionsList = NULL, CreatedTime = F, ErrorField = F, ErrorNumber = F, ErrorText = F, ModifiedTime = F, TempMassUpdateGeolocationAddressErrorID = F, TempMassUpdateGeolocationAddressID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "TempMassUpdateGeolocationAddressError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassUpdateGeolocationAddressError
	#'
	#' This function returns a dataframe or json object of a TempMassUpdateGeolocationAddressError
	#' @param TempMassUpdateGeolocationAddressErrorID The ID of the TempMassUpdateGeolocationAddressError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassUpdateGeolocationAddressError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassUpdateGeolocationAddressError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassUpdateGeolocationAddressError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of TempMassUpdateGeolocationAddressError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassUpdateGeolocationAddressError <- function(TempMassUpdateGeolocationAddressErrorID, CreatedTime = F, ErrorField = F, ErrorNumber = F, ErrorText = F, ModifiedTime = F, TempMassUpdateGeolocationAddressID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassUpdateGeolocationAddressErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "TempMassUpdateGeolocationAddressError", objectId = TempMassUpdateGeolocationAddressErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassUpdateGeolocationAddressError
	#'
	#' This function deletes a TempMassUpdateGeolocationAddressError
	#' @param TempMassUpdateGeolocationAddressErrorID The ID of the TempMassUpdateGeolocationAddressError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The TempMassUpdateGeolocationAddressErrorID of the deleted TempMassUpdateGeolocationAddressError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassUpdateGeolocationAddressError <- function(TempMassUpdateGeolocationAddressErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "TempMassUpdateGeolocationAddressError", objectId = TempMassUpdateGeolocationAddressErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassUpdateGeolocationAddressError
	#'
	#' This function creates a TempMassUpdateGeolocationAddressError
	#' @param fieldNames The field values to give the created TempMassUpdateGeolocationAddressError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created TempMassUpdateGeolocationAddressError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassUpdateGeolocationAddressError <- function(ErrorField = NULL, ErrorNumber = NULL, ErrorText = NULL, TempMassUpdateGeolocationAddressID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "TempMassUpdateGeolocationAddressError", body = list(DataObject = body), searchFields = append("TempMassUpdateGeolocationAddressErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassUpdateGeolocationAddressError
	#'
	#' This function modifies a TempMassUpdateGeolocationAddressError
	#' @param fieldNames The field values to give the modified TempMassUpdateGeolocationAddressError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified TempMassUpdateGeolocationAddressError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassUpdateGeolocationAddressError <- function(TempMassUpdateGeolocationAddressErrorID, ErrorField = NULL, ErrorNumber = NULL, ErrorText = NULL, TempMassUpdateGeolocationAddressID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "TempMassUpdateGeolocationAddressError", objectId = TempMassUpdateGeolocationAddressErrorID, body = list(DataObject = body), searchFields = append("TempMassUpdateGeolocationAddressErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AddressMoveHistories
	#'
	#' This function returns a dataframe or json object of AddressMoveHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AddressMoveHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AddressMoveHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AddressMoveHistory') to get more field paths.
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
	#' @concept Demographics
	#' @return A list of AddressMoveHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAddressMoveHistories <- function(searchConditionsList = NULL, AddressID = F, AddressMoveHistoryID = F, CreatedTime = F, ModifiedTime = F, MoveInDate = F, MoveOutDate = F, NameID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Demographics", objectName = "AddressMoveHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AddressMoveHistory
	#'
	#' This function returns a dataframe or json object of an AddressMoveHistory
	#' @param AddressMoveHistoryID The ID of the AddressMoveHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AddressMoveHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AddressMoveHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AddressMoveHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A dataframe or of AddressMoveHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAddressMoveHistory <- function(AddressMoveHistoryID, AddressID = F, CreatedTime = F, ModifiedTime = F, MoveInDate = F, MoveOutDate = F, NameID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AddressMoveHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Demographics", objectName = "AddressMoveHistory", objectId = AddressMoveHistoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AddressMoveHistory
	#'
	#' This function deletes an AddressMoveHistory
	#' @param AddressMoveHistoryID The ID of the AddressMoveHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The AddressMoveHistoryID of the deleted AddressMoveHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAddressMoveHistory <- function(AddressMoveHistoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Demographics", objectName = "AddressMoveHistory", objectId = AddressMoveHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AddressMoveHistory
	#'
	#' This function creates an AddressMoveHistory
	#' @param fieldNames The field values to give the created AddressMoveHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return A newly created AddressMoveHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAddressMoveHistory <- function(AddressID = NULL, MoveInDate = NULL, MoveOutDate = NULL, NameID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Demographics", objectName = "AddressMoveHistory", body = list(DataObject = body), searchFields = append("AddressMoveHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AddressMoveHistory
	#'
	#' This function modifies an AddressMoveHistory
	#' @param fieldNames The field values to give the modified AddressMoveHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Demographics
	#' @return The modified AddressMoveHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAddressMoveHistory <- function(AddressMoveHistoryID, AddressID = NULL, MoveInDate = NULL, MoveOutDate = NULL, NameID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Demographics", objectName = "AddressMoveHistory", objectId = AddressMoveHistoryID, body = list(DataObject = body), searchFields = append("AddressMoveHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
