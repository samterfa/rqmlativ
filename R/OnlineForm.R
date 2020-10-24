
	#' List StaffContacts
	#'
	#' This function returns a dataframe or json object of StaffContacts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffContacts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffContacts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffContact') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of StaffContacts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStaffContacts <- function(searchConditionsList = NULL, StaffContactID = F, OnlineFormID = F, StaffID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "StaffContact", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StaffContact
	#'
	#' This function returns a dataframe or json object of a StaffContact
	#' @param StaffContactID The ID of the StaffContact to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffContact. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffContact.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffContact') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of StaffContact
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStaffContact <- function(StaffContactID, OnlineFormID = F, StaffID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StaffContactID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "StaffContact", objectId = StaffContactID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StaffContact
	#'
	#' This function deletes a StaffContact
	#' @param StaffContactID The ID of the StaffContact to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The StaffContactID of the deleted StaffContact.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStaffContact <- function(StaffContactID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "StaffContact", objectId = StaffContactID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StaffContact
	#'
	#' This function creates a StaffContact
	#' @param fieldNames The field values to give the created StaffContact. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created StaffContact
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStaffContact <- function(OnlineFormID = NULL, StaffID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "StaffContact", body = list(DataObject = body), searchFields = append("StaffContactID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StaffContact
	#'
	#' This function modifies a StaffContact
	#' @param fieldNames The field values to give the modified StaffContact. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified StaffContact
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStaffContact <- function(StaffContactID, OnlineFormID = NULL, StaffID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "StaffContact", objectId = StaffContactID, body = list(DataObject = body), searchFields = append("StaffContactID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OnlineFormTypes
	#'
	#' This function returns a dataframe or json object of OnlineFormTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormType') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of OnlineFormTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOnlineFormTypes <- function(searchConditionsList = NULL, OnlineFormTypeID = F, Code = F, Description = F, LimitUsersToOneFormOfThisTypePerYear = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "OnlineFormType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OnlineFormType
	#'
	#' This function returns a dataframe or json object of an OnlineFormType
	#' @param OnlineFormTypeID The ID of the OnlineFormType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of OnlineFormType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOnlineFormType <- function(OnlineFormTypeID, Code = F, Description = F, LimitUsersToOneFormOfThisTypePerYear = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OnlineFormTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "OnlineFormType", objectId = OnlineFormTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OnlineFormType
	#'
	#' This function deletes an OnlineFormType
	#' @param OnlineFormTypeID The ID of the OnlineFormType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The OnlineFormTypeID of the deleted OnlineFormType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOnlineFormType <- function(OnlineFormTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "OnlineFormType", objectId = OnlineFormTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OnlineFormType
	#'
	#' This function creates an OnlineFormType
	#' @param fieldNames The field values to give the created OnlineFormType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created OnlineFormType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOnlineFormType <- function(Code = NULL, Description = NULL, LimitUsersToOneFormOfThisTypePerYear = NULL, DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "OnlineFormType", body = list(DataObject = body), searchFields = append("OnlineFormTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OnlineFormType
	#'
	#' This function modifies an OnlineFormType
	#' @param fieldNames The field values to give the modified OnlineFormType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified OnlineFormType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOnlineFormType <- function(OnlineFormTypeID, Code = NULL, Description = NULL, LimitUsersToOneFormOfThisTypePerYear = NULL, DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "OnlineFormType", objectId = OnlineFormTypeID, body = list(DataObject = body), searchFields = append("OnlineFormTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OnlineFormTempCertifications
	#'
	#' This function returns a dataframe or json object of OnlineFormTempCertifications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormTempCertifications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormTempCertifications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormTempCertification') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of OnlineFormTempCertifications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOnlineFormTempCertifications <- function(searchConditionsList = NULL, TempCertificationID = F, CertificationID = F, EmployeeID = F, CertificationTypeID = F, InstitutionID = F, StateID = F, CertificationNumber = F, IssueDate = F, ExpirationDate = F, IsDelete = F, OnScreenCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "TempCertification", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OnlineFormTempCertification
	#'
	#' This function returns a dataframe or json object of an OnlineFormTempCertification
	#' @param OnlineFormTempCertificationID The ID of the OnlineFormTempCertification to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormTempCertification. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormTempCertification.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormTempCertification') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of OnlineFormTempCertification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOnlineFormTempCertification <- function(OnlineFormTempCertificationID, TempCertificationID = F, CertificationID = F, EmployeeID = F, CertificationTypeID = F, InstitutionID = F, StateID = F, CertificationNumber = F, IssueDate = F, ExpirationDate = F, IsDelete = F, OnScreenCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OnlineFormTempCertificationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "TempCertification", objectId = OnlineFormTempCertificationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OnlineFormTempCertification
	#'
	#' This function deletes an OnlineFormTempCertification
	#' @param OnlineFormTempCertificationID The ID of the OnlineFormTempCertification to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The OnlineFormTempCertificationID of the deleted OnlineFormTempCertification.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOnlineFormTempCertification <- function(OnlineFormTempCertificationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "TempCertification", objectId = OnlineFormTempCertificationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OnlineFormTempCertification
	#'
	#' This function creates an OnlineFormTempCertification
	#' @param fieldNames The field values to give the created OnlineFormTempCertification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created OnlineFormTempCertification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOnlineFormTempCertification <- function(CertificationID = NULL, EmployeeID = NULL, CertificationTypeID = NULL, InstitutionID = NULL, StateID = NULL, CertificationNumber = NULL, IssueDate = NULL, ExpirationDate = NULL, IsDelete = NULL, OnScreenCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "TempCertification", body = list(DataObject = body), searchFields = append("TempCertificationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OnlineFormTempCertification
	#'
	#' This function modifies an OnlineFormTempCertification
	#' @param fieldNames The field values to give the modified OnlineFormTempCertification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified OnlineFormTempCertification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOnlineFormTempCertification <- function(TempCertificationID, CertificationID = NULL, EmployeeID = NULL, CertificationTypeID = NULL, InstitutionID = NULL, StateID = NULL, CertificationNumber = NULL, IssueDate = NULL, ExpirationDate = NULL, IsDelete = NULL, OnScreenCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "TempCertification", objectId = TempCertificationID, body = list(DataObject = body), searchFields = append("TempCertificationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDegrees
	#'
	#' This function returns a dataframe or json object of TempDegrees
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDegrees. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDegrees.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDegree') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of TempDegrees
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDegrees <- function(searchConditionsList = NULL, TempDegreeID = F, DegreeID = F, EmployeeID = F, DegreeTypeID = F, InstitutionID = F, ReceivedDate = F, ApprovedDate = F, Credits = F, GPA = F, AdditionalCredits = F, IsDelete = F, OnScreenCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "TempDegree", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDegree
	#'
	#' This function returns a dataframe or json object of a TempDegree
	#' @param TempDegreeID The ID of the TempDegree to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDegree. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDegree.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDegree') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of TempDegree
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDegree <- function(TempDegreeID, DegreeID = F, EmployeeID = F, DegreeTypeID = F, InstitutionID = F, ReceivedDate = F, ApprovedDate = F, Credits = F, GPA = F, AdditionalCredits = F, IsDelete = F, OnScreenCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDegreeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "TempDegree", objectId = TempDegreeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDegree
	#'
	#' This function deletes a TempDegree
	#' @param TempDegreeID The ID of the TempDegree to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The TempDegreeID of the deleted TempDegree.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDegree <- function(TempDegreeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "TempDegree", objectId = TempDegreeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDegree
	#'
	#' This function creates a TempDegree
	#' @param fieldNames The field values to give the created TempDegree. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created TempDegree
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDegree <- function(DegreeID = NULL, EmployeeID = NULL, DegreeTypeID = NULL, InstitutionID = NULL, ReceivedDate = NULL, ApprovedDate = NULL, Credits = NULL, GPA = NULL, AdditionalCredits = NULL, IsDelete = NULL, OnScreenCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "TempDegree", body = list(DataObject = body), searchFields = append("TempDegreeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDegree
	#'
	#' This function modifies a TempDegree
	#' @param fieldNames The field values to give the modified TempDegree. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified TempDegree
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDegree <- function(TempDegreeID, DegreeID = NULL, EmployeeID = NULL, DegreeTypeID = NULL, InstitutionID = NULL, ReceivedDate = NULL, ApprovedDate = NULL, Credits = NULL, GPA = NULL, AdditionalCredits = NULL, IsDelete = NULL, OnScreenCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "TempDegree", objectId = TempDegreeID, body = list(DataObject = body), searchFields = append("TempDegreeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ElementStatuses
	#'
	#' This function returns a dataframe or json object of ElementStatuses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ElementStatuses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ElementStatuses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ElementStatus') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of ElementStatuses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listElementStatuses <- function(searchConditionsList = NULL, ElementStatusID = F, StepStatusID = F, ElementID = F, UserSubmitted = F, IsReadOnly = F, StatusType = F, DenialMessage = F, ValidationMessage = F, OriginalValue = F, RequestedValue = F, UserIDApprover = F, MediaIDAttachment = F, SharedElementStatusID = F, NeedsAdminReviewSave = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "ElementStatus", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ElementStatus
	#'
	#' This function returns a dataframe or json object of an ElementStatus
	#' @param ElementStatusID The ID of the ElementStatus to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ElementStatus. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ElementStatus.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ElementStatus') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of ElementStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getElementStatus <- function(ElementStatusID, StepStatusID = F, ElementID = F, UserSubmitted = F, IsReadOnly = F, StatusType = F, DenialMessage = F, ValidationMessage = F, OriginalValue = F, RequestedValue = F, UserIDApprover = F, MediaIDAttachment = F, SharedElementStatusID = F, NeedsAdminReviewSave = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ElementStatusID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "ElementStatus", objectId = ElementStatusID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ElementStatus
	#'
	#' This function deletes an ElementStatus
	#' @param ElementStatusID The ID of the ElementStatus to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The ElementStatusID of the deleted ElementStatus.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteElementStatus <- function(ElementStatusID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "ElementStatus", objectId = ElementStatusID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ElementStatus
	#'
	#' This function creates an ElementStatus
	#' @param fieldNames The field values to give the created ElementStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created ElementStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createElementStatus <- function(StepStatusID = NULL, ElementID = NULL, UserSubmitted = NULL, IsReadOnly = NULL, StatusType = NULL, DenialMessage = NULL, ValidationMessage = NULL, OriginalValue = NULL, RequestedValue = NULL, UserIDApprover = NULL, MediaIDAttachment = NULL, SharedElementStatusID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "ElementStatus", body = list(DataObject = body), searchFields = append("ElementStatusID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ElementStatus
	#'
	#' This function modifies an ElementStatus
	#' @param fieldNames The field values to give the modified ElementStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified ElementStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyElementStatus <- function(ElementStatusID, StepStatusID = NULL, ElementID = NULL, UserSubmitted = NULL, IsReadOnly = NULL, StatusType = NULL, DenialMessage = NULL, ValidationMessage = NULL, OriginalValue = NULL, RequestedValue = NULL, UserIDApprover = NULL, MediaIDAttachment = NULL, SharedElementStatusID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "ElementStatus", objectId = ElementStatusID, body = list(DataObject = body), searchFields = append("ElementStatusID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NewStudentEnrollmentGuardianData
	#'
	#' This function returns a dataframe or json object of NewStudentEnrollmentGuardianData
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NewStudentEnrollmentGuardianData. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NewStudentEnrollmentGuardianData.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NewStudentEnrollmentGuardianData') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of NewStudentEnrollmentGuardianData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNewStudentEnrollmentGuardianData <- function(searchConditionsList = NULL, NewStudentEnrollmentGuardianDataID = F, FamilyGuardianID = F, NameID = F, LastName = F, FirstName = F, MiddleName = F, NameSuffixID = F, GenderCode = F, RelationshipID = F, AllowStudentPickup = F, DriversLicenseNumber = F, DeleteGuardian = F, OnScreenID = F, Rank = F, CreateNewGuardian = F, AddGuardian = F, NameVehicleID = F, VehicleID = F, Year = F, MakeModel = F, Color = F, LicensePlateNumber = F, VIN = F, IsCustodialGuardian = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "NewStudentEnrollmentGuardianData", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NewStudentEnrollmentGuardianData
	#'
	#' This function returns a dataframe or json object of a NewStudentEnrollmentGuardianData
	#' @param NewStudentEnrollmentGuardianDataID The ID of the NewStudentEnrollmentGuardianData to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NewStudentEnrollmentGuardianData. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NewStudentEnrollmentGuardianData.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NewStudentEnrollmentGuardianData') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of NewStudentEnrollmentGuardianData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNewStudentEnrollmentGuardianData <- function(NewStudentEnrollmentGuardianDataID, FamilyGuardianID = F, NameID = F, LastName = F, FirstName = F, MiddleName = F, NameSuffixID = F, GenderCode = F, RelationshipID = F, AllowStudentPickup = F, DriversLicenseNumber = F, DeleteGuardian = F, OnScreenID = F, Rank = F, CreateNewGuardian = F, AddGuardian = F, NameVehicleID = F, VehicleID = F, Year = F, MakeModel = F, Color = F, LicensePlateNumber = F, VIN = F, IsCustodialGuardian = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NewStudentEnrollmentGuardianDataID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "NewStudentEnrollmentGuardianData", objectId = NewStudentEnrollmentGuardianDataID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NewStudentEnrollmentGuardianData
	#'
	#' This function deletes a NewStudentEnrollmentGuardianData
	#' @param NewStudentEnrollmentGuardianDataID The ID of the NewStudentEnrollmentGuardianData to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The NewStudentEnrollmentGuardianDataID of the deleted NewStudentEnrollmentGuardianData.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNewStudentEnrollmentGuardianData <- function(NewStudentEnrollmentGuardianDataID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "NewStudentEnrollmentGuardianData", objectId = NewStudentEnrollmentGuardianDataID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NewStudentEnrollmentGuardianData
	#'
	#' This function creates a NewStudentEnrollmentGuardianData
	#' @param fieldNames The field values to give the created NewStudentEnrollmentGuardianData. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created NewStudentEnrollmentGuardianData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNewStudentEnrollmentGuardianData <- function(FamilyGuardianID = NULL, NameID = NULL, LastName = NULL, FirstName = NULL, MiddleName = NULL, NameSuffixID = NULL, GenderCode = NULL, RelationshipID = NULL, AllowStudentPickup = NULL, DriversLicenseNumber = NULL, DeleteGuardian = NULL, OnScreenID = NULL, Rank = NULL, CreateNewGuardian = NULL, AddGuardian = NULL, NameVehicleID = NULL, VehicleID = NULL, Year = NULL, MakeModel = NULL, Color = NULL, LicensePlateNumber = NULL, VIN = NULL, IsCustodialGuardian = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "NewStudentEnrollmentGuardianData", body = list(DataObject = body), searchFields = append("NewStudentEnrollmentGuardianDataID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NewStudentEnrollmentGuardianData
	#'
	#' This function modifies a NewStudentEnrollmentGuardianData
	#' @param fieldNames The field values to give the modified NewStudentEnrollmentGuardianData. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified NewStudentEnrollmentGuardianData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNewStudentEnrollmentGuardianData <- function(NewStudentEnrollmentGuardianDataID, FamilyGuardianID = NULL, NameID = NULL, LastName = NULL, FirstName = NULL, MiddleName = NULL, NameSuffixID = NULL, GenderCode = NULL, RelationshipID = NULL, AllowStudentPickup = NULL, DriversLicenseNumber = NULL, DeleteGuardian = NULL, OnScreenID = NULL, Rank = NULL, CreateNewGuardian = NULL, AddGuardian = NULL, NameVehicleID = NULL, VehicleID = NULL, Year = NULL, MakeModel = NULL, Color = NULL, LicensePlateNumber = NULL, VIN = NULL, IsCustodialGuardian = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "NewStudentEnrollmentGuardianData", objectId = NewStudentEnrollmentGuardianDataID, body = list(DataObject = body), searchFields = append("NewStudentEnrollmentGuardianDataID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NewStudentEnrollmentUserData
	#'
	#' This function returns a dataframe or json object of NewStudentEnrollmentUserData
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NewStudentEnrollmentUserData. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NewStudentEnrollmentUserData.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NewStudentEnrollmentUserData') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of NewStudentEnrollmentUserData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNewStudentEnrollmentUserData <- function(searchConditionsList = NULL, NewStudentEnrollmentUserDataID = F, UserIDSubmittedBy = F, PhysicalStreetNumber = F, PhysicalStreetName = F, UnitNumber = F, Unit = F, ZipCode = F, City = F, State = F, EmailAddress = F, PhoneNumber = F, PreviouslyInDistrict = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "NewStudentEnrollmentUserData", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NewStudentEnrollmentUserData
	#'
	#' This function returns a dataframe or json object of a NewStudentEnrollmentUserData
	#' @param NewStudentEnrollmentUserDataID The ID of the NewStudentEnrollmentUserData to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NewStudentEnrollmentUserData. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NewStudentEnrollmentUserData.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NewStudentEnrollmentUserData') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of NewStudentEnrollmentUserData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNewStudentEnrollmentUserData <- function(NewStudentEnrollmentUserDataID, UserIDSubmittedBy = F, PhysicalStreetNumber = F, PhysicalStreetName = F, UnitNumber = F, Unit = F, ZipCode = F, City = F, State = F, EmailAddress = F, PhoneNumber = F, PreviouslyInDistrict = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NewStudentEnrollmentUserDataID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "NewStudentEnrollmentUserData", objectId = NewStudentEnrollmentUserDataID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NewStudentEnrollmentUserData
	#'
	#' This function deletes a NewStudentEnrollmentUserData
	#' @param NewStudentEnrollmentUserDataID The ID of the NewStudentEnrollmentUserData to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The NewStudentEnrollmentUserDataID of the deleted NewStudentEnrollmentUserData.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNewStudentEnrollmentUserData <- function(NewStudentEnrollmentUserDataID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "NewStudentEnrollmentUserData", objectId = NewStudentEnrollmentUserDataID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NewStudentEnrollmentUserData
	#'
	#' This function creates a NewStudentEnrollmentUserData
	#' @param fieldNames The field values to give the created NewStudentEnrollmentUserData. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created NewStudentEnrollmentUserData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNewStudentEnrollmentUserData <- function(UserIDSubmittedBy = NULL, PhysicalStreetNumber = NULL, PhysicalStreetName = NULL, UnitNumber = NULL, Unit = NULL, ZipCode = NULL, City = NULL, State = NULL, EmailAddress = NULL, PhoneNumber = NULL, PreviouslyInDistrict = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "NewStudentEnrollmentUserData", body = list(DataObject = body), searchFields = append("NewStudentEnrollmentUserDataID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NewStudentEnrollmentUserData
	#'
	#' This function modifies a NewStudentEnrollmentUserData
	#' @param fieldNames The field values to give the modified NewStudentEnrollmentUserData. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified NewStudentEnrollmentUserData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNewStudentEnrollmentUserData <- function(NewStudentEnrollmentUserDataID, UserIDSubmittedBy = NULL, PhysicalStreetNumber = NULL, PhysicalStreetName = NULL, UnitNumber = NULL, Unit = NULL, ZipCode = NULL, City = NULL, State = NULL, EmailAddress = NULL, PhoneNumber = NULL, PreviouslyInDistrict = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "NewStudentEnrollmentUserData", objectId = NewStudentEnrollmentUserDataID, body = list(DataObject = body), searchFields = append("NewStudentEnrollmentUserDataID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OnlineFormStatuses
	#'
	#' This function returns a dataframe or json object of OnlineFormStatuses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormStatuses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormStatuses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormStatus') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of OnlineFormStatuses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOnlineFormStatuses <- function(searchConditionsList = NULL, OnlineFormStatusID = F, NameID = F, SecondaryID = F, StatusType = F, SubmittedDateTime = F, DenialMessage = F, DenialDateTime = F, CompletedByAdmin = F, UserIDApprover = F, UserIDSubmittedBy = F, ApprovalRevoked = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OnlineFormEntityID = F, IsOutsideAddressRanges = F, WithinCutoffTime = F, FullNameLFMSubmittedForOverride = F, FullNameLFMSubmittedFor = F, IsOutsideAddressRangeSchoolPaths = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "OnlineFormStatus", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OnlineFormStatus
	#'
	#' This function returns a dataframe or json object of an OnlineFormStatus
	#' @param OnlineFormStatusID The ID of the OnlineFormStatus to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormStatus. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormStatus.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormStatus') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of OnlineFormStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOnlineFormStatus <- function(OnlineFormStatusID, NameID = F, SecondaryID = F, StatusType = F, SubmittedDateTime = F, DenialMessage = F, DenialDateTime = F, CompletedByAdmin = F, UserIDApprover = F, UserIDSubmittedBy = F, ApprovalRevoked = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OnlineFormEntityID = F, IsOutsideAddressRanges = F, WithinCutoffTime = F, FullNameLFMSubmittedForOverride = F, FullNameLFMSubmittedFor = F, IsOutsideAddressRangeSchoolPaths = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OnlineFormStatusID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "OnlineFormStatus", objectId = OnlineFormStatusID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OnlineFormStatus
	#'
	#' This function deletes an OnlineFormStatus
	#' @param OnlineFormStatusID The ID of the OnlineFormStatus to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The OnlineFormStatusID of the deleted OnlineFormStatus.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOnlineFormStatus <- function(OnlineFormStatusID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "OnlineFormStatus", objectId = OnlineFormStatusID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OnlineFormStatus
	#'
	#' This function creates an OnlineFormStatus
	#' @param fieldNames The field values to give the created OnlineFormStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created OnlineFormStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOnlineFormStatus <- function(NameID = NULL, SecondaryID = NULL, StatusType = NULL, SubmittedDateTime = NULL, DenialMessage = NULL, DenialDateTime = NULL, CompletedByAdmin = NULL, UserIDApprover = NULL, UserIDSubmittedBy = NULL, ApprovalRevoked = NULL, OnlineFormEntityID = NULL, IsOutsideAddressRanges = NULL, IsOutsideAddressRangeSchoolPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "OnlineFormStatus", body = list(DataObject = body), searchFields = append("OnlineFormStatusID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OnlineFormStatus
	#'
	#' This function modifies an OnlineFormStatus
	#' @param fieldNames The field values to give the modified OnlineFormStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified OnlineFormStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOnlineFormStatus <- function(OnlineFormStatusID, NameID = NULL, SecondaryID = NULL, StatusType = NULL, SubmittedDateTime = NULL, DenialMessage = NULL, DenialDateTime = NULL, CompletedByAdmin = NULL, UserIDApprover = NULL, UserIDSubmittedBy = NULL, ApprovalRevoked = NULL, OnlineFormEntityID = NULL, IsOutsideAddressRanges = NULL, IsOutsideAddressRangeSchoolPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "OnlineFormStatus", objectId = OnlineFormStatusID, body = list(DataObject = body), searchFields = append("OnlineFormStatusID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SharedElementStatuses
	#'
	#' This function returns a dataframe or json object of SharedElementStatuses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SharedElementStatuses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SharedElementStatuses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SharedElementStatus') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of SharedElementStatuses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSharedElementStatuses <- function(searchConditionsList = NULL, SharedElementStatusID = F, SchoolYearID = F, SecondaryID = F, NameID = F, MediaIDAttachment = F, ElementType = F, FieldGroupType = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "SharedElementStatus", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SharedElementStatus
	#'
	#' This function returns a dataframe or json object of a SharedElementStatus
	#' @param SharedElementStatusID The ID of the SharedElementStatus to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SharedElementStatus. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SharedElementStatus.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SharedElementStatus') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of SharedElementStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSharedElementStatus <- function(SharedElementStatusID, SchoolYearID = F, SecondaryID = F, NameID = F, MediaIDAttachment = F, ElementType = F, FieldGroupType = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SharedElementStatusID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "SharedElementStatus", objectId = SharedElementStatusID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SharedElementStatus
	#'
	#' This function deletes a SharedElementStatus
	#' @param SharedElementStatusID The ID of the SharedElementStatus to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The SharedElementStatusID of the deleted SharedElementStatus.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSharedElementStatus <- function(SharedElementStatusID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "SharedElementStatus", objectId = SharedElementStatusID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SharedElementStatus
	#'
	#' This function creates a SharedElementStatus
	#' @param fieldNames The field values to give the created SharedElementStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created SharedElementStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSharedElementStatus <- function(SchoolYearID = NULL, SecondaryID = NULL, NameID = NULL, MediaIDAttachment = NULL, ElementType = NULL, FieldGroupType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "SharedElementStatus", body = list(DataObject = body), searchFields = append("SharedElementStatusID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SharedElementStatus
	#'
	#' This function modifies a SharedElementStatus
	#' @param fieldNames The field values to give the modified SharedElementStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified SharedElementStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySharedElementStatus <- function(SharedElementStatusID, SchoolYearID = NULL, SecondaryID = NULL, NameID = NULL, MediaIDAttachment = NULL, ElementType = NULL, FieldGroupType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "SharedElementStatus", objectId = SharedElementStatusID, body = list(DataObject = body), searchFields = append("SharedElementStatusID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StepStatuses
	#'
	#' This function returns a dataframe or json object of StepStatuses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StepStatuses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StepStatuses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StepStatus') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of StepStatuses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStepStatuses <- function(searchConditionsList = NULL, StepStatusID = F, OnlineFormStatusID = F, StepID = F, StatusType = F, DenialMessage = F, ValidationMessage = F, PreviousStepStatusID = F, NextStepStatusID = F, HasPreviousStepStatus = F, HasNextStepStatus = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "StepStatus", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StepStatus
	#'
	#' This function returns a dataframe or json object of a StepStatus
	#' @param StepStatusID The ID of the StepStatus to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StepStatus. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StepStatus.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StepStatus') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of StepStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStepStatus <- function(StepStatusID, OnlineFormStatusID = F, StepID = F, StatusType = F, DenialMessage = F, ValidationMessage = F, PreviousStepStatusID = F, NextStepStatusID = F, HasPreviousStepStatus = F, HasNextStepStatus = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StepStatusID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "StepStatus", objectId = StepStatusID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StepStatus
	#'
	#' This function deletes a StepStatus
	#' @param StepStatusID The ID of the StepStatus to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The StepStatusID of the deleted StepStatus.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStepStatus <- function(StepStatusID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "StepStatus", objectId = StepStatusID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StepStatus
	#'
	#' This function creates a StepStatus
	#' @param fieldNames The field values to give the created StepStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created StepStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStepStatus <- function(OnlineFormStatusID = NULL, StepID = NULL, StatusType = NULL, DenialMessage = NULL, ValidationMessage = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "StepStatus", body = list(DataObject = body), searchFields = append("StepStatusID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StepStatus
	#'
	#' This function modifies a StepStatus
	#' @param fieldNames The field values to give the modified StepStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified StepStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStepStatus <- function(StepStatusID, OnlineFormStatusID = NULL, StepID = NULL, StatusType = NULL, DenialMessage = NULL, ValidationMessage = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "StepStatus", objectId = StepStatusID, body = list(DataObject = body), searchFields = append("StepStatusID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OnlineFormTempEmergencyContacts
	#'
	#' This function returns a dataframe or json object of OnlineFormTempEmergencyContacts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormTempEmergencyContacts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormTempEmergencyContacts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormTempEmergencyContact') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of OnlineFormTempEmergencyContacts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOnlineFormTempEmergencyContacts <- function(searchConditionsList = NULL, TempEmergencyContactID = F, EmergencyContactID = F, Rank = F, NameID = F, FirstName = F, MiddleName = F, LastName = F, RelationshipID = F, AllowStudentPickup = F, DriversLicenseNumber = F, Comment = F, PrimaryPhoneNamePhoneID = F, PrimaryPhonePhoneTypeID = F, PrimaryPhonePhoneNumber = F, PrimaryPhoneExtension = F, PrimaryPhonePreventFamilyStudentAccessUpdates = F, SecondPhoneNamePhoneID = F, SecondPhonePhoneTypeID = F, SecondPhonePhoneNumber = F, SecondPhoneExtension = F, SecondPhonePreventFamilyStudentAccessUpdates = F, ThirdPhoneNamePhoneID = F, ThirdPhonePhoneTypeID = F, ThirdPhonePhoneNumber = F, ThirdPhoneExtension = F, ThirdPhonePreventFamilyStudentAccessUpdates = F, PrimaryEmailNameEmailID = F, PrimaryEmailEmailTypeID = F, PrimaryEmailEmailAddress = F, PrimaryEmailPreventFamilyStudentAccessUpdates = F, SecondEmailNameEmailID = F, SecondEmailEmailTypeID = F, SecondEmailEmailAddress = F, SecondEmailPreventFamilyStudentAccessUpdates = F, ThirdEmailNameEmailID = F, ThirdEmailEmailTypeID = F, ThirdEmailEmailAddress = F, ThirdEmailPreventFamilyStudentAccessUpdates = F, OnScreenID = F, CreateNewEmergencyContactName = F, DeleteEmergencyContact = F, AddEmergencyContact = F, IsAlsoGuardian = F, ForSecondFamily = F, IsBusiness = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsHealthProfessionalName = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "TempEmergencyContact", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OnlineFormTempEmergencyContact
	#'
	#' This function returns a dataframe or json object of an OnlineFormTempEmergencyContact
	#' @param OnlineFormTempEmergencyContactID The ID of the OnlineFormTempEmergencyContact to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormTempEmergencyContact. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormTempEmergencyContact.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormTempEmergencyContact') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of OnlineFormTempEmergencyContact
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOnlineFormTempEmergencyContact <- function(OnlineFormTempEmergencyContactID, TempEmergencyContactID = F, EmergencyContactID = F, Rank = F, NameID = F, FirstName = F, MiddleName = F, LastName = F, RelationshipID = F, AllowStudentPickup = F, DriversLicenseNumber = F, Comment = F, PrimaryPhoneNamePhoneID = F, PrimaryPhonePhoneTypeID = F, PrimaryPhonePhoneNumber = F, PrimaryPhoneExtension = F, PrimaryPhonePreventFamilyStudentAccessUpdates = F, SecondPhoneNamePhoneID = F, SecondPhonePhoneTypeID = F, SecondPhonePhoneNumber = F, SecondPhoneExtension = F, SecondPhonePreventFamilyStudentAccessUpdates = F, ThirdPhoneNamePhoneID = F, ThirdPhonePhoneTypeID = F, ThirdPhonePhoneNumber = F, ThirdPhoneExtension = F, ThirdPhonePreventFamilyStudentAccessUpdates = F, PrimaryEmailNameEmailID = F, PrimaryEmailEmailTypeID = F, PrimaryEmailEmailAddress = F, PrimaryEmailPreventFamilyStudentAccessUpdates = F, SecondEmailNameEmailID = F, SecondEmailEmailTypeID = F, SecondEmailEmailAddress = F, SecondEmailPreventFamilyStudentAccessUpdates = F, ThirdEmailNameEmailID = F, ThirdEmailEmailTypeID = F, ThirdEmailEmailAddress = F, ThirdEmailPreventFamilyStudentAccessUpdates = F, OnScreenID = F, CreateNewEmergencyContactName = F, DeleteEmergencyContact = F, AddEmergencyContact = F, IsAlsoGuardian = F, ForSecondFamily = F, IsBusiness = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsHealthProfessionalName = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OnlineFormTempEmergencyContactID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "TempEmergencyContact", objectId = OnlineFormTempEmergencyContactID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OnlineFormTempEmergencyContact
	#'
	#' This function deletes an OnlineFormTempEmergencyContact
	#' @param OnlineFormTempEmergencyContactID The ID of the OnlineFormTempEmergencyContact to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The OnlineFormTempEmergencyContactID of the deleted OnlineFormTempEmergencyContact.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOnlineFormTempEmergencyContact <- function(OnlineFormTempEmergencyContactID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "TempEmergencyContact", objectId = OnlineFormTempEmergencyContactID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OnlineFormTempEmergencyContact
	#'
	#' This function creates an OnlineFormTempEmergencyContact
	#' @param fieldNames The field values to give the created OnlineFormTempEmergencyContact. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created OnlineFormTempEmergencyContact
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOnlineFormTempEmergencyContact <- function(EmergencyContactID = NULL, Rank = NULL, NameID = NULL, FirstName = NULL, MiddleName = NULL, LastName = NULL, RelationshipID = NULL, AllowStudentPickup = NULL, DriversLicenseNumber = NULL, Comment = NULL, PrimaryPhoneNamePhoneID = NULL, PrimaryPhonePhoneTypeID = NULL, PrimaryPhonePhoneNumber = NULL, PrimaryPhoneExtension = NULL, PrimaryPhonePreventFamilyStudentAccessUpdates = NULL, SecondPhoneNamePhoneID = NULL, SecondPhonePhoneTypeID = NULL, SecondPhonePhoneNumber = NULL, SecondPhoneExtension = NULL, SecondPhonePreventFamilyStudentAccessUpdates = NULL, ThirdPhoneNamePhoneID = NULL, ThirdPhonePhoneTypeID = NULL, ThirdPhonePhoneNumber = NULL, ThirdPhoneExtension = NULL, ThirdPhonePreventFamilyStudentAccessUpdates = NULL, PrimaryEmailNameEmailID = NULL, PrimaryEmailEmailTypeID = NULL, PrimaryEmailEmailAddress = NULL, PrimaryEmailPreventFamilyStudentAccessUpdates = NULL, SecondEmailNameEmailID = NULL, SecondEmailEmailTypeID = NULL, SecondEmailEmailAddress = NULL, SecondEmailPreventFamilyStudentAccessUpdates = NULL, ThirdEmailNameEmailID = NULL, ThirdEmailEmailTypeID = NULL, ThirdEmailEmailAddress = NULL, ThirdEmailPreventFamilyStudentAccessUpdates = NULL, OnScreenID = NULL, CreateNewEmergencyContactName = NULL, DeleteEmergencyContact = NULL, AddEmergencyContact = NULL, IsAlsoGuardian = NULL, ForSecondFamily = NULL, IsBusiness = NULL, IsHealthProfessionalName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "TempEmergencyContact", body = list(DataObject = body), searchFields = append("TempEmergencyContactID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OnlineFormTempEmergencyContact
	#'
	#' This function modifies an OnlineFormTempEmergencyContact
	#' @param fieldNames The field values to give the modified OnlineFormTempEmergencyContact. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified OnlineFormTempEmergencyContact
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOnlineFormTempEmergencyContact <- function(TempEmergencyContactID, EmergencyContactID = NULL, Rank = NULL, NameID = NULL, FirstName = NULL, MiddleName = NULL, LastName = NULL, RelationshipID = NULL, AllowStudentPickup = NULL, DriversLicenseNumber = NULL, Comment = NULL, PrimaryPhoneNamePhoneID = NULL, PrimaryPhonePhoneTypeID = NULL, PrimaryPhonePhoneNumber = NULL, PrimaryPhoneExtension = NULL, PrimaryPhonePreventFamilyStudentAccessUpdates = NULL, SecondPhoneNamePhoneID = NULL, SecondPhonePhoneTypeID = NULL, SecondPhonePhoneNumber = NULL, SecondPhoneExtension = NULL, SecondPhonePreventFamilyStudentAccessUpdates = NULL, ThirdPhoneNamePhoneID = NULL, ThirdPhonePhoneTypeID = NULL, ThirdPhonePhoneNumber = NULL, ThirdPhoneExtension = NULL, ThirdPhonePreventFamilyStudentAccessUpdates = NULL, PrimaryEmailNameEmailID = NULL, PrimaryEmailEmailTypeID = NULL, PrimaryEmailEmailAddress = NULL, PrimaryEmailPreventFamilyStudentAccessUpdates = NULL, SecondEmailNameEmailID = NULL, SecondEmailEmailTypeID = NULL, SecondEmailEmailAddress = NULL, SecondEmailPreventFamilyStudentAccessUpdates = NULL, ThirdEmailNameEmailID = NULL, ThirdEmailEmailTypeID = NULL, ThirdEmailEmailAddress = NULL, ThirdEmailPreventFamilyStudentAccessUpdates = NULL, OnScreenID = NULL, CreateNewEmergencyContactName = NULL, DeleteEmergencyContact = NULL, AddEmergencyContact = NULL, IsAlsoGuardian = NULL, ForSecondFamily = NULL, IsBusiness = NULL, IsHealthProfessionalName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "TempEmergencyContact", objectId = TempEmergencyContactID, body = list(DataObject = body), searchFields = append("TempEmergencyContactID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempNewStudentEnrollmentGuardianPhones
	#'
	#' This function returns a dataframe or json object of TempNewStudentEnrollmentGuardianPhones
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNewStudentEnrollmentGuardianPhones. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNewStudentEnrollmentGuardianPhones.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNewStudentEnrollmentGuardianPhone') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of TempNewStudentEnrollmentGuardianPhones
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempNewStudentEnrollmentGuardianPhones <- function(searchConditionsList = NULL, TempNewStudentEnrollmentGuardianPhoneID = F, NamePhoneID = F, PhoneTypeID = F, PhoneNumber = F, Extension = F, PhonePreventFamilyStudentAccessUpdates = F, Rank = F, OnScreenID = F, ParentOnScreenID = F, CreateNewPhone = F, DeletePhone = F, AddPhone = F, IsConfidential = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "TempNewStudentEnrollmentGuardianPhone", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempNewStudentEnrollmentGuardianPhone
	#'
	#' This function returns a dataframe or json object of a TempNewStudentEnrollmentGuardianPhone
	#' @param TempNewStudentEnrollmentGuardianPhoneID The ID of the TempNewStudentEnrollmentGuardianPhone to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNewStudentEnrollmentGuardianPhone. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNewStudentEnrollmentGuardianPhone.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNewStudentEnrollmentGuardianPhone') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of TempNewStudentEnrollmentGuardianPhone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempNewStudentEnrollmentGuardianPhone <- function(TempNewStudentEnrollmentGuardianPhoneID, NamePhoneID = F, PhoneTypeID = F, PhoneNumber = F, Extension = F, PhonePreventFamilyStudentAccessUpdates = F, Rank = F, OnScreenID = F, ParentOnScreenID = F, CreateNewPhone = F, DeletePhone = F, AddPhone = F, IsConfidential = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempNewStudentEnrollmentGuardianPhoneID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "TempNewStudentEnrollmentGuardianPhone", objectId = TempNewStudentEnrollmentGuardianPhoneID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempNewStudentEnrollmentGuardianPhone
	#'
	#' This function deletes a TempNewStudentEnrollmentGuardianPhone
	#' @param TempNewStudentEnrollmentGuardianPhoneID The ID of the TempNewStudentEnrollmentGuardianPhone to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The TempNewStudentEnrollmentGuardianPhoneID of the deleted TempNewStudentEnrollmentGuardianPhone.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempNewStudentEnrollmentGuardianPhone <- function(TempNewStudentEnrollmentGuardianPhoneID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "TempNewStudentEnrollmentGuardianPhone", objectId = TempNewStudentEnrollmentGuardianPhoneID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempNewStudentEnrollmentGuardianPhone
	#'
	#' This function creates a TempNewStudentEnrollmentGuardianPhone
	#' @param fieldNames The field values to give the created TempNewStudentEnrollmentGuardianPhone. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created TempNewStudentEnrollmentGuardianPhone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempNewStudentEnrollmentGuardianPhone <- function(NamePhoneID = NULL, PhoneTypeID = NULL, PhoneNumber = NULL, Extension = NULL, PhonePreventFamilyStudentAccessUpdates = NULL, Rank = NULL, OnScreenID = NULL, ParentOnScreenID = NULL, CreateNewPhone = NULL, DeletePhone = NULL, AddPhone = NULL, IsConfidential = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "TempNewStudentEnrollmentGuardianPhone", body = list(DataObject = body), searchFields = append("TempNewStudentEnrollmentGuardianPhoneID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempNewStudentEnrollmentGuardianPhone
	#'
	#' This function modifies a TempNewStudentEnrollmentGuardianPhone
	#' @param fieldNames The field values to give the modified TempNewStudentEnrollmentGuardianPhone. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified TempNewStudentEnrollmentGuardianPhone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempNewStudentEnrollmentGuardianPhone <- function(TempNewStudentEnrollmentGuardianPhoneID, NamePhoneID = NULL, PhoneTypeID = NULL, PhoneNumber = NULL, Extension = NULL, PhonePreventFamilyStudentAccessUpdates = NULL, Rank = NULL, OnScreenID = NULL, ParentOnScreenID = NULL, CreateNewPhone = NULL, DeletePhone = NULL, AddPhone = NULL, IsConfidential = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "TempNewStudentEnrollmentGuardianPhone", objectId = TempNewStudentEnrollmentGuardianPhoneID, body = list(DataObject = body), searchFields = append("TempNewStudentEnrollmentGuardianPhoneID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempNewStudentEnrollmentGuardianEmails
	#'
	#' This function returns a dataframe or json object of TempNewStudentEnrollmentGuardianEmails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNewStudentEnrollmentGuardianEmails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNewStudentEnrollmentGuardianEmails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNewStudentEnrollmentGuardianEmail') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of TempNewStudentEnrollmentGuardianEmails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempNewStudentEnrollmentGuardianEmails <- function(searchConditionsList = NULL, TempNewStudentEnrollmentGuardianEmailID = F, NameEmailID = F, EmailTypeID = F, EmailAddress = F, EmailPreventFamilyStudentAccessUpdates = F, OnScreenID = F, ParentOnScreenID = F, CreateNewEmail = F, DeleteEmail = F, AddEmail = F, Rank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "TempNewStudentEnrollmentGuardianEmail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempNewStudentEnrollmentGuardianEmail
	#'
	#' This function returns a dataframe or json object of a TempNewStudentEnrollmentGuardianEmail
	#' @param TempNewStudentEnrollmentGuardianEmailID The ID of the TempNewStudentEnrollmentGuardianEmail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNewStudentEnrollmentGuardianEmail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNewStudentEnrollmentGuardianEmail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNewStudentEnrollmentGuardianEmail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of TempNewStudentEnrollmentGuardianEmail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempNewStudentEnrollmentGuardianEmail <- function(TempNewStudentEnrollmentGuardianEmailID, NameEmailID = F, EmailTypeID = F, EmailAddress = F, EmailPreventFamilyStudentAccessUpdates = F, OnScreenID = F, ParentOnScreenID = F, CreateNewEmail = F, DeleteEmail = F, AddEmail = F, Rank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempNewStudentEnrollmentGuardianEmailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "TempNewStudentEnrollmentGuardianEmail", objectId = TempNewStudentEnrollmentGuardianEmailID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempNewStudentEnrollmentGuardianEmail
	#'
	#' This function deletes a TempNewStudentEnrollmentGuardianEmail
	#' @param TempNewStudentEnrollmentGuardianEmailID The ID of the TempNewStudentEnrollmentGuardianEmail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The TempNewStudentEnrollmentGuardianEmailID of the deleted TempNewStudentEnrollmentGuardianEmail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempNewStudentEnrollmentGuardianEmail <- function(TempNewStudentEnrollmentGuardianEmailID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "TempNewStudentEnrollmentGuardianEmail", objectId = TempNewStudentEnrollmentGuardianEmailID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempNewStudentEnrollmentGuardianEmail
	#'
	#' This function creates a TempNewStudentEnrollmentGuardianEmail
	#' @param fieldNames The field values to give the created TempNewStudentEnrollmentGuardianEmail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created TempNewStudentEnrollmentGuardianEmail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempNewStudentEnrollmentGuardianEmail <- function(NameEmailID = NULL, EmailTypeID = NULL, EmailAddress = NULL, EmailPreventFamilyStudentAccessUpdates = NULL, OnScreenID = NULL, ParentOnScreenID = NULL, CreateNewEmail = NULL, DeleteEmail = NULL, AddEmail = NULL, Rank = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "TempNewStudentEnrollmentGuardianEmail", body = list(DataObject = body), searchFields = append("TempNewStudentEnrollmentGuardianEmailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempNewStudentEnrollmentGuardianEmail
	#'
	#' This function modifies a TempNewStudentEnrollmentGuardianEmail
	#' @param fieldNames The field values to give the modified TempNewStudentEnrollmentGuardianEmail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified TempNewStudentEnrollmentGuardianEmail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempNewStudentEnrollmentGuardianEmail <- function(TempNewStudentEnrollmentGuardianEmailID, NameEmailID = NULL, EmailTypeID = NULL, EmailAddress = NULL, EmailPreventFamilyStudentAccessUpdates = NULL, OnScreenID = NULL, ParentOnScreenID = NULL, CreateNewEmail = NULL, DeleteEmail = NULL, AddEmail = NULL, Rank = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "TempNewStudentEnrollmentGuardianEmail", objectId = TempNewStudentEnrollmentGuardianEmailID, body = list(DataObject = body), searchFields = append("TempNewStudentEnrollmentGuardianEmailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSteps
	#'
	#' This function returns a dataframe or json object of TempSteps
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSteps. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSteps.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStep') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of TempSteps
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSteps <- function(searchConditionsList = NULL, TempStepID = F, EntityGroupKey = F, StepID = F, OnlineFormID = F, Name = F, Message = F, IsRequired = F, IsActive = F, Order = F, IsReadOnlyForNonPrimaryFamily = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ErrorCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "TempStep", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStep
	#'
	#' This function returns a dataframe or json object of a TempStep
	#' @param TempStepID The ID of the TempStep to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStep. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStep.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStep') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of TempStep
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStep <- function(TempStepID, EntityGroupKey = F, StepID = F, OnlineFormID = F, Name = F, Message = F, IsRequired = F, IsActive = F, Order = F, IsReadOnlyForNonPrimaryFamily = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ErrorCount = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStepID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "TempStep", objectId = TempStepID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStep
	#'
	#' This function deletes a TempStep
	#' @param TempStepID The ID of the TempStep to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The TempStepID of the deleted TempStep.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStep <- function(TempStepID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "TempStep", objectId = TempStepID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStep
	#'
	#' This function creates a TempStep
	#' @param fieldNames The field values to give the created TempStep. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created TempStep
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStep <- function(EntityGroupKey = NULL, StepID = NULL, OnlineFormID = NULL, Name = NULL, Message = NULL, IsRequired = NULL, IsActive = NULL, Order = NULL, IsReadOnlyForNonPrimaryFamily = NULL, ErrorCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "TempStep", body = list(DataObject = body), searchFields = append("TempStepID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStep
	#'
	#' This function modifies a TempStep
	#' @param fieldNames The field values to give the modified TempStep. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified TempStep
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStep <- function(TempStepID, EntityGroupKey = NULL, StepID = NULL, OnlineFormID = NULL, Name = NULL, Message = NULL, IsRequired = NULL, IsActive = NULL, Order = NULL, IsReadOnlyForNonPrimaryFamily = NULL, ErrorCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "TempStep", objectId = TempStepID, body = list(DataObject = body), searchFields = append("TempStepID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Elements
	#'
	#' This function returns a dataframe or json object of Elements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Elements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Elements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Element') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of Elements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listElements <- function(searchConditionsList = NULL, ElementID = F, StepID = F, MediaID = F, FieldID = F, Type = F, ParameterData = F, Description = F, Order = F, AllowsUserInteraction = F, HasBackingObject = F, AdminCanEdit = F, HasSharedValue = F, SharedValueUsesNotShared = F, IsNewStudentEnrollmentElement = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RenderWeight = F, FieldRelationship = F, SupportsLabelOverrides = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "Element", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Element
	#'
	#' This function returns a dataframe or json object of an Element
	#' @param ElementID The ID of the Element to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Element. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Element.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Element') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of Element
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getElement <- function(ElementID, StepID = F, MediaID = F, FieldID = F, Type = F, ParameterData = F, Description = F, Order = F, AllowsUserInteraction = F, HasBackingObject = F, AdminCanEdit = F, HasSharedValue = F, SharedValueUsesNotShared = F, IsNewStudentEnrollmentElement = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RenderWeight = F, FieldRelationship = F, SupportsLabelOverrides = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ElementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "Element", objectId = ElementID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Element
	#'
	#' This function deletes an Element
	#' @param ElementID The ID of the Element to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The ElementID of the deleted Element.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteElement <- function(ElementID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "Element", objectId = ElementID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Element
	#'
	#' This function creates an Element
	#' @param fieldNames The field values to give the created Element. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created Element
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createElement <- function(StepID = NULL, MediaID = NULL, FieldID = NULL, Type = NULL, ParameterData = NULL, Order = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "Element", body = list(DataObject = body), searchFields = append("ElementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Element
	#'
	#' This function modifies an Element
	#' @param fieldNames The field values to give the modified Element. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified Element
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyElement <- function(ElementID, StepID = NULL, MediaID = NULL, FieldID = NULL, Type = NULL, ParameterData = NULL, Order = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "Element", objectId = ElementID, body = list(DataObject = body), searchFields = append("ElementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Steps
	#'
	#' This function returns a dataframe or json object of Steps
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Steps. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Steps.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Step') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of Steps
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSteps <- function(searchConditionsList = NULL, StepID = F, OnlineFormID = F, Name = F, Message = F, IsRequired = F, IsActive = F, Order = F, IsReadOnlyForNonPrimaryFamily = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "Step", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Step
	#'
	#' This function returns a dataframe or json object of a Step
	#' @param StepID The ID of the Step to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Step. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Step.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Step') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of Step
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStep <- function(StepID, OnlineFormID = F, Name = F, Message = F, IsRequired = F, IsActive = F, Order = F, IsReadOnlyForNonPrimaryFamily = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StepID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "Step", objectId = StepID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Step
	#'
	#' This function deletes a Step
	#' @param StepID The ID of the Step to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The StepID of the deleted Step.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStep <- function(StepID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "Step", objectId = StepID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Step
	#'
	#' This function creates a Step
	#' @param fieldNames The field values to give the created Step. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created Step
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStep <- function(OnlineFormID = NULL, Name = NULL, Message = NULL, IsRequired = NULL, IsActive = NULL, Order = NULL, IsReadOnlyForNonPrimaryFamily = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "Step", body = list(DataObject = body), searchFields = append("StepID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Step
	#'
	#' This function modifies a Step
	#' @param fieldNames The field values to give the modified Step. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified Step
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStep <- function(StepID, OnlineFormID = NULL, Name = NULL, Message = NULL, IsRequired = NULL, IsActive = NULL, Order = NULL, IsReadOnlyForNonPrimaryFamily = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "Step", objectId = StepID, body = list(DataObject = body), searchFields = append("StepID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OnlineFormDateExceptions
	#'
	#' This function returns a dataframe or json object of OnlineFormDateExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormDateExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormDateExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormDateException') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of OnlineFormDateExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOnlineFormDateExceptions <- function(searchConditionsList = NULL, OnlineFormDateExceptionID = F, OnlineFormID = F, ObjectID = F, ObjectPrimaryKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "OnlineFormDateException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OnlineFormDateException
	#'
	#' This function returns a dataframe or json object of an OnlineFormDateException
	#' @param OnlineFormDateExceptionID The ID of the OnlineFormDateException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormDateException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormDateException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormDateException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of OnlineFormDateException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOnlineFormDateException <- function(OnlineFormDateExceptionID, OnlineFormID = F, ObjectID = F, ObjectPrimaryKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OnlineFormDateExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "OnlineFormDateException", objectId = OnlineFormDateExceptionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OnlineFormDateException
	#'
	#' This function deletes an OnlineFormDateException
	#' @param OnlineFormDateExceptionID The ID of the OnlineFormDateException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The OnlineFormDateExceptionID of the deleted OnlineFormDateException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOnlineFormDateException <- function(OnlineFormDateExceptionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "OnlineFormDateException", objectId = OnlineFormDateExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OnlineFormDateException
	#'
	#' This function creates an OnlineFormDateException
	#' @param fieldNames The field values to give the created OnlineFormDateException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created OnlineFormDateException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOnlineFormDateException <- function(OnlineFormID = NULL, ObjectID = NULL, ObjectPrimaryKey = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "OnlineFormDateException", body = list(DataObject = body), searchFields = append("OnlineFormDateExceptionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OnlineFormDateException
	#'
	#' This function modifies an OnlineFormDateException
	#' @param fieldNames The field values to give the modified OnlineFormDateException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified OnlineFormDateException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOnlineFormDateException <- function(OnlineFormDateExceptionID, OnlineFormID = NULL, ObjectID = NULL, ObjectPrimaryKey = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "OnlineFormDateException", objectId = OnlineFormDateExceptionID, body = list(DataObject = body), searchFields = append("OnlineFormDateExceptionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OnlineForms
	#'
	#' This function returns a dataframe or json object of OnlineForms
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineForms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineForms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineForm') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of OnlineForms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOnlineForms <- function(searchConditionsList = NULL, OnlineFormID = F, Name = F, SchoolYearID = F, OnlineFormTypeID = F, Description = F, ShowDescriptionOnTile = F, Filter = F, Module = F, Object = F, IncludeInStudentPortal = F, IncludeInFamilyPortal = F, IncludeInAdministrativePortal = F, IncludeInEmployeePortal = F, IncludeInTeacherPortal = F, IncludeInActivitiesPortal = F, IncludeInNewStudentEnrollmentPortal = F, IsPublished = F, StartDate = F, EndDate = F, Message = F, ReviewStepMessage = F, MessageAfterSubmission = F, DisplayForNonPrimaryFamily = F, IncludeAdobeAcrobatLink = F, IconCode = F, OnlineFormIDClonedFrom = F, MessageForNonPrimaryFamily = F, PortalsIncludedIn = F, MainPageEmbeddedLinkProtocol = F, MainPageEmbeddedLinkUrl = F, InUseByUsers = F, ContactHash = F, AllowContact = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowMultipleFormsPerUser = F, Portals = F, NoApprovalNeeded = F, SendFamilyAccessEmail = F, HideCurrentValueOnApproval = F, SkipInstructionsPage = F, SkipThankYouPage = F, YearType = F, FilterSubType = F, PortalType = F, IsUserInitiated = F, CutoffTime = F, WithinCutoffTime = F, LimitToOnePerDay = F, OnlineFormStatusExistsToday = F, SkipReviewPage = F, ThankYouPageLinkProtocol = F, ThankYouPageLinkUrl = F, ThankYouPageLinkUrlDisplayName = F, IncludeInReporting = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "OnlineForm", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OnlineForm
	#'
	#' This function returns a dataframe or json object of an OnlineForm
	#' @param OnlineFormID The ID of the OnlineForm to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineForm. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineForm.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineForm') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of OnlineForm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOnlineForm <- function(OnlineFormID, Name = F, SchoolYearID = F, OnlineFormTypeID = F, Description = F, ShowDescriptionOnTile = F, Filter = F, Module = F, Object = F, IncludeInStudentPortal = F, IncludeInFamilyPortal = F, IncludeInAdministrativePortal = F, IncludeInEmployeePortal = F, IncludeInTeacherPortal = F, IncludeInActivitiesPortal = F, IncludeInNewStudentEnrollmentPortal = F, IsPublished = F, StartDate = F, EndDate = F, Message = F, ReviewStepMessage = F, MessageAfterSubmission = F, DisplayForNonPrimaryFamily = F, IncludeAdobeAcrobatLink = F, IconCode = F, OnlineFormIDClonedFrom = F, MessageForNonPrimaryFamily = F, PortalsIncludedIn = F, MainPageEmbeddedLinkProtocol = F, MainPageEmbeddedLinkUrl = F, InUseByUsers = F, ContactHash = F, AllowContact = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowMultipleFormsPerUser = F, Portals = F, NoApprovalNeeded = F, SendFamilyAccessEmail = F, HideCurrentValueOnApproval = F, SkipInstructionsPage = F, SkipThankYouPage = F, YearType = F, FilterSubType = F, PortalType = F, IsUserInitiated = F, CutoffTime = F, WithinCutoffTime = F, LimitToOnePerDay = F, OnlineFormStatusExistsToday = F, SkipReviewPage = F, ThankYouPageLinkProtocol = F, ThankYouPageLinkUrl = F, ThankYouPageLinkUrlDisplayName = F, IncludeInReporting = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OnlineFormID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "OnlineForm", objectId = OnlineFormID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OnlineForm
	#'
	#' This function deletes an OnlineForm
	#' @param OnlineFormID The ID of the OnlineForm to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The OnlineFormID of the deleted OnlineForm.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOnlineForm <- function(OnlineFormID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "OnlineForm", objectId = OnlineFormID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OnlineForm
	#'
	#' This function creates an OnlineForm
	#' @param fieldNames The field values to give the created OnlineForm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created OnlineForm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOnlineForm <- function(Name = NULL, SchoolYearID = NULL, OnlineFormTypeID = NULL, Description = NULL, ShowDescriptionOnTile = NULL, Filter = NULL, IsPublished = NULL, StartDate = NULL, EndDate = NULL, Message = NULL, ReviewStepMessage = NULL, MessageAfterSubmission = NULL, DisplayForNonPrimaryFamily = NULL, IncludeAdobeAcrobatLink = NULL, IconCode = NULL, OnlineFormIDClonedFrom = NULL, MessageForNonPrimaryFamily = NULL, MainPageEmbeddedLinkProtocol = NULL, MainPageEmbeddedLinkUrl = NULL, AllowContact = NULL, AllowMultipleFormsPerUser = NULL, NoApprovalNeeded = NULL, SendFamilyAccessEmail = NULL, HideCurrentValueOnApproval = NULL, SkipInstructionsPage = NULL, SkipThankYouPage = NULL, YearType = NULL, FilterSubType = NULL, PortalType = NULL, IsUserInitiated = NULL, CutoffTime = NULL, LimitToOnePerDay = NULL, SkipReviewPage = NULL, ThankYouPageLinkProtocol = NULL, ThankYouPageLinkUrl = NULL, ThankYouPageLinkUrlDisplayName = NULL, IncludeInReporting = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "OnlineForm", body = list(DataObject = body), searchFields = append("OnlineFormID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OnlineForm
	#'
	#' This function modifies an OnlineForm
	#' @param fieldNames The field values to give the modified OnlineForm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified OnlineForm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOnlineForm <- function(OnlineFormID, Name = NULL, SchoolYearID = NULL, OnlineFormTypeID = NULL, Description = NULL, ShowDescriptionOnTile = NULL, Filter = NULL, IsPublished = NULL, StartDate = NULL, EndDate = NULL, Message = NULL, ReviewStepMessage = NULL, MessageAfterSubmission = NULL, DisplayForNonPrimaryFamily = NULL, IncludeAdobeAcrobatLink = NULL, IconCode = NULL, OnlineFormIDClonedFrom = NULL, MessageForNonPrimaryFamily = NULL, MainPageEmbeddedLinkProtocol = NULL, MainPageEmbeddedLinkUrl = NULL, AllowContact = NULL, AllowMultipleFormsPerUser = NULL, NoApprovalNeeded = NULL, SendFamilyAccessEmail = NULL, HideCurrentValueOnApproval = NULL, SkipInstructionsPage = NULL, SkipThankYouPage = NULL, YearType = NULL, FilterSubType = NULL, PortalType = NULL, IsUserInitiated = NULL, CutoffTime = NULL, LimitToOnePerDay = NULL, SkipReviewPage = NULL, ThankYouPageLinkProtocol = NULL, ThankYouPageLinkUrl = NULL, ThankYouPageLinkUrlDisplayName = NULL, IncludeInReporting = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "OnlineForm", objectId = OnlineFormID, body = list(DataObject = body), searchFields = append("OnlineFormID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ElementGroups
	#'
	#' This function returns a dataframe or json object of ElementGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ElementGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ElementGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ElementGroup') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of ElementGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listElementGroups <- function(searchConditionsList = NULL, ElementGroupID = F, SkywardID = F, Name = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "ElementGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ElementGroup
	#'
	#' This function returns a dataframe or json object of an ElementGroup
	#' @param ElementGroupID The ID of the ElementGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ElementGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ElementGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ElementGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of ElementGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getElementGroup <- function(ElementGroupID, SkywardID = F, Name = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ElementGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "ElementGroup", objectId = ElementGroupID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ElementGroup
	#'
	#' This function deletes an ElementGroup
	#' @param ElementGroupID The ID of the ElementGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The ElementGroupID of the deleted ElementGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteElementGroup <- function(ElementGroupID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "ElementGroup", objectId = ElementGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ElementGroup
	#'
	#' This function creates an ElementGroup
	#' @param fieldNames The field values to give the created ElementGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created ElementGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createElementGroup <- function(Name = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "ElementGroup", body = list(DataObject = body), searchFields = append("ElementGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ElementGroup
	#'
	#' This function modifies an ElementGroup
	#' @param fieldNames The field values to give the modified ElementGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified ElementGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyElementGroup <- function(ElementGroupID, Name = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "ElementGroup", objectId = ElementGroupID, body = list(DataObject = body), searchFields = append("ElementGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ElementGroupTemplates
	#'
	#' This function returns a dataframe or json object of ElementGroupTemplates
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ElementGroupTemplates. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ElementGroupTemplates.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ElementGroupTemplate') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of ElementGroupTemplates
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listElementGroupTemplates <- function(searchConditionsList = NULL, ElementGroupTemplateID = F, SkywardID = F, ElementGroupID = F, Type = F, ParameterData = F, Order = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "ElementGroupTemplate", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ElementGroupTemplate
	#'
	#' This function returns a dataframe or json object of an ElementGroupTemplate
	#' @param ElementGroupTemplateID The ID of the ElementGroupTemplate to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ElementGroupTemplate. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ElementGroupTemplate.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ElementGroupTemplate') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of ElementGroupTemplate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getElementGroupTemplate <- function(ElementGroupTemplateID, SkywardID = F, ElementGroupID = F, Type = F, ParameterData = F, Order = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ElementGroupTemplateID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "ElementGroupTemplate", objectId = ElementGroupTemplateID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ElementGroupTemplate
	#'
	#' This function deletes an ElementGroupTemplate
	#' @param ElementGroupTemplateID The ID of the ElementGroupTemplate to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The ElementGroupTemplateID of the deleted ElementGroupTemplate.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteElementGroupTemplate <- function(ElementGroupTemplateID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "ElementGroupTemplate", objectId = ElementGroupTemplateID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ElementGroupTemplate
	#'
	#' This function creates an ElementGroupTemplate
	#' @param fieldNames The field values to give the created ElementGroupTemplate. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created ElementGroupTemplate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createElementGroupTemplate <- function(ElementGroupID = NULL, Type = NULL, ParameterData = NULL, Order = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "ElementGroupTemplate", body = list(DataObject = body), searchFields = append("ElementGroupTemplateID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ElementGroupTemplate
	#'
	#' This function modifies an ElementGroupTemplate
	#' @param fieldNames The field values to give the modified ElementGroupTemplate. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified ElementGroupTemplate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyElementGroupTemplate <- function(ElementGroupTemplateID, ElementGroupID = NULL, Type = NULL, ParameterData = NULL, Order = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "ElementGroupTemplate", objectId = ElementGroupTemplateID, body = list(DataObject = body), searchFields = append("ElementGroupTemplateID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OnlineFormEntities
	#'
	#' This function returns a dataframe or json object of OnlineFormEntities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormEntities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormEntities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormEntity') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of OnlineFormEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOnlineFormEntities <- function(searchConditionsList = NULL, OnlineFormEntityID = F, OnlineFormID = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "OnlineFormEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OnlineFormEntity
	#'
	#' This function returns a dataframe or json object of an OnlineFormEntity
	#' @param OnlineFormEntityID The ID of the OnlineFormEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormEntity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormEntity.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of OnlineFormEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOnlineFormEntity <- function(OnlineFormEntityID, OnlineFormID = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OnlineFormEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "OnlineFormEntity", objectId = OnlineFormEntityID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OnlineFormEntity
	#'
	#' This function deletes an OnlineFormEntity
	#' @param OnlineFormEntityID The ID of the OnlineFormEntity to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The OnlineFormEntityID of the deleted OnlineFormEntity.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOnlineFormEntity <- function(OnlineFormEntityID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "OnlineFormEntity", objectId = OnlineFormEntityID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OnlineFormEntity
	#'
	#' This function creates an OnlineFormEntity
	#' @param fieldNames The field values to give the created OnlineFormEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created OnlineFormEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOnlineFormEntity <- function(OnlineFormID = NULL, EntityID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "OnlineFormEntity", body = list(DataObject = body), searchFields = append("OnlineFormEntityID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OnlineFormEntity
	#'
	#' This function modifies an OnlineFormEntity
	#' @param fieldNames The field values to give the modified OnlineFormEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified OnlineFormEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOnlineFormEntity <- function(OnlineFormEntityID, OnlineFormID = NULL, EntityID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "OnlineFormEntity", objectId = OnlineFormEntityID, body = list(DataObject = body), searchFields = append("OnlineFormEntityID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OnlineFormStatusNames
	#'
	#' This function returns a dataframe or json object of OnlineFormStatusNames
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormStatusNames. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormStatusNames.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormStatusName') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of OnlineFormStatusNames
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOnlineFormStatusNames <- function(searchConditionsList = NULL, OnlineFormStatusNameID = F, OnlineFormStatusID = F, NameID = F, ElementID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "OnlineFormStatusName", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OnlineFormStatusName
	#'
	#' This function returns a dataframe or json object of an OnlineFormStatusName
	#' @param OnlineFormStatusNameID The ID of the OnlineFormStatusName to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormStatusName. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormStatusName.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormStatusName') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of OnlineFormStatusName
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOnlineFormStatusName <- function(OnlineFormStatusNameID, OnlineFormStatusID = F, NameID = F, ElementID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OnlineFormStatusNameID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "OnlineFormStatusName", objectId = OnlineFormStatusNameID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OnlineFormStatusName
	#'
	#' This function deletes an OnlineFormStatusName
	#' @param OnlineFormStatusNameID The ID of the OnlineFormStatusName to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The OnlineFormStatusNameID of the deleted OnlineFormStatusName.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOnlineFormStatusName <- function(OnlineFormStatusNameID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "OnlineFormStatusName", objectId = OnlineFormStatusNameID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OnlineFormStatusName
	#'
	#' This function creates an OnlineFormStatusName
	#' @param fieldNames The field values to give the created OnlineFormStatusName. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created OnlineFormStatusName
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOnlineFormStatusName <- function(OnlineFormStatusID = NULL, NameID = NULL, ElementID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "OnlineFormStatusName", body = list(DataObject = body), searchFields = append("OnlineFormStatusNameID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OnlineFormStatusName
	#'
	#' This function modifies an OnlineFormStatusName
	#' @param fieldNames The field values to give the modified OnlineFormStatusName. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified OnlineFormStatusName
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOnlineFormStatusName <- function(OnlineFormStatusNameID, OnlineFormStatusID = NULL, NameID = NULL, ElementID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "OnlineFormStatusName", objectId = OnlineFormStatusNameID, body = list(DataObject = body), searchFields = append("OnlineFormStatusNameID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MassPrintHistories
	#'
	#' This function returns a dataframe or json object of MassPrintHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MassPrintHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MassPrintHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MassPrintHistory') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of MassPrintHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMassPrintHistories <- function(searchConditionsList = NULL, MassPrintHistoryID = F, OnlineFormID = F, MediaID = F, WorkflowInstanceID = F, RequestIdentifier = F, SendMessageOnComplete = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "MassPrintHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MassPrintHistory
	#'
	#' This function returns a dataframe or json object of a MassPrintHistory
	#' @param MassPrintHistoryID The ID of the MassPrintHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MassPrintHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MassPrintHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MassPrintHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of MassPrintHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMassPrintHistory <- function(MassPrintHistoryID, OnlineFormID = F, MediaID = F, WorkflowInstanceID = F, RequestIdentifier = F, SendMessageOnComplete = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MassPrintHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "MassPrintHistory", objectId = MassPrintHistoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MassPrintHistory
	#'
	#' This function deletes a MassPrintHistory
	#' @param MassPrintHistoryID The ID of the MassPrintHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The MassPrintHistoryID of the deleted MassPrintHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMassPrintHistory <- function(MassPrintHistoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "MassPrintHistory", objectId = MassPrintHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MassPrintHistory
	#'
	#' This function creates a MassPrintHistory
	#' @param fieldNames The field values to give the created MassPrintHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created MassPrintHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMassPrintHistory <- function(OnlineFormID = NULL, MediaID = NULL, WorkflowInstanceID = NULL, SendMessageOnComplete = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "MassPrintHistory", body = list(DataObject = body), searchFields = append("MassPrintHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MassPrintHistory
	#'
	#' This function modifies a MassPrintHistory
	#' @param fieldNames The field values to give the modified MassPrintHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified MassPrintHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMassPrintHistory <- function(MassPrintHistoryID, OnlineFormID = NULL, MediaID = NULL, WorkflowInstanceID = NULL, SendMessageOnComplete = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "MassPrintHistory", objectId = MassPrintHistoryID, body = list(DataObject = body), searchFields = append("MassPrintHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ElementStatusSurveyAnswers
	#'
	#' This function returns a dataframe or json object of ElementStatusSurveyAnswers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ElementStatusSurveyAnswers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ElementStatusSurveyAnswers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ElementStatusSurveyAnswer') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of ElementStatusSurveyAnswers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listElementStatusSurveyAnswers <- function(searchConditionsList = NULL, ElementStatusSurveyAnswerID = F, ElementStatusID = F, OnlineFormID = F, NameID = F, ColumnName = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "ElementStatusSurveyAnswer", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ElementStatusSurveyAnswer
	#'
	#' This function returns a dataframe or json object of an ElementStatusSurveyAnswer
	#' @param ElementStatusSurveyAnswerID The ID of the ElementStatusSurveyAnswer to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ElementStatusSurveyAnswer. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ElementStatusSurveyAnswer.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ElementStatusSurveyAnswer') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of ElementStatusSurveyAnswer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getElementStatusSurveyAnswer <- function(ElementStatusSurveyAnswerID, ElementStatusID = F, OnlineFormID = F, NameID = F, ColumnName = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ElementStatusSurveyAnswerID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "ElementStatusSurveyAnswer", objectId = ElementStatusSurveyAnswerID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ElementStatusSurveyAnswer
	#'
	#' This function deletes an ElementStatusSurveyAnswer
	#' @param ElementStatusSurveyAnswerID The ID of the ElementStatusSurveyAnswer to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The ElementStatusSurveyAnswerID of the deleted ElementStatusSurveyAnswer.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteElementStatusSurveyAnswer <- function(ElementStatusSurveyAnswerID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "ElementStatusSurveyAnswer", objectId = ElementStatusSurveyAnswerID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ElementStatusSurveyAnswer
	#'
	#' This function creates an ElementStatusSurveyAnswer
	#' @param fieldNames The field values to give the created ElementStatusSurveyAnswer. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created ElementStatusSurveyAnswer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createElementStatusSurveyAnswer <- function(ElementStatusID = NULL, OnlineFormID = NULL, NameID = NULL, ColumnName = NULL, Value = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "ElementStatusSurveyAnswer", body = list(DataObject = body), searchFields = append("ElementStatusSurveyAnswerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ElementStatusSurveyAnswer
	#'
	#' This function modifies an ElementStatusSurveyAnswer
	#' @param fieldNames The field values to give the modified ElementStatusSurveyAnswer. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified ElementStatusSurveyAnswer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyElementStatusSurveyAnswer <- function(ElementStatusSurveyAnswerID, ElementStatusID = NULL, OnlineFormID = NULL, NameID = NULL, ColumnName = NULL, Value = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "ElementStatusSurveyAnswer", objectId = ElementStatusSurveyAnswerID, body = list(DataObject = body), searchFields = append("ElementStatusSurveyAnswerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDependents
	#'
	#' This function returns a dataframe or json object of TempDependents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDependents. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDependents.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDependent') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of TempDependents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDependents <- function(searchConditionsList = NULL, TempDependentID = F, LastName = F, FirstName = F, MiddleName = F, BirthDate = F, IsExistingRecord = F, IsDelete = F, OnScreenCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DependentID = F, RelationshipID = F, SocialSecurityNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "TempDependent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDependent
	#'
	#' This function returns a dataframe or json object of a TempDependent
	#' @param TempDependentID The ID of the TempDependent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDependent. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDependent.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDependent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of TempDependent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDependent <- function(TempDependentID, LastName = F, FirstName = F, MiddleName = F, BirthDate = F, IsExistingRecord = F, IsDelete = F, OnScreenCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DependentID = F, RelationshipID = F, SocialSecurityNumber = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDependentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "TempDependent", objectId = TempDependentID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDependent
	#'
	#' This function deletes a TempDependent
	#' @param TempDependentID The ID of the TempDependent to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The TempDependentID of the deleted TempDependent.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDependent <- function(TempDependentID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "TempDependent", objectId = TempDependentID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDependent
	#'
	#' This function creates a TempDependent
	#' @param fieldNames The field values to give the created TempDependent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created TempDependent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDependent <- function(LastName = NULL, FirstName = NULL, MiddleName = NULL, BirthDate = NULL, IsExistingRecord = NULL, IsDelete = NULL, OnScreenCount = NULL, DependentID = NULL, RelationshipID = NULL, SocialSecurityNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "TempDependent", body = list(DataObject = body), searchFields = append("TempDependentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDependent
	#'
	#' This function modifies a TempDependent
	#' @param fieldNames The field values to give the modified TempDependent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified TempDependent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDependent <- function(TempDependentID, LastName = NULL, FirstName = NULL, MiddleName = NULL, BirthDate = NULL, IsExistingRecord = NULL, IsDelete = NULL, OnScreenCount = NULL, DependentID = NULL, RelationshipID = NULL, SocialSecurityNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "TempDependent", objectId = TempDependentID, body = list(DataObject = body), searchFields = append("TempDependentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OnlineFormClearances
	#'
	#' This function returns a dataframe or json object of OnlineFormClearances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormClearances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormClearances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormClearance') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of OnlineFormClearances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOnlineFormClearances <- function(searchConditionsList = NULL, OnlineFormClearanceID = F, OnlineFormID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "OnlineFormClearance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OnlineFormClearance
	#'
	#' This function returns a dataframe or json object of an OnlineFormClearance
	#' @param OnlineFormClearanceID The ID of the OnlineFormClearance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormClearance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormClearance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormClearance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of OnlineFormClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOnlineFormClearance <- function(OnlineFormClearanceID, OnlineFormID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OnlineFormClearanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "OnlineFormClearance", objectId = OnlineFormClearanceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OnlineFormClearance
	#'
	#' This function deletes an OnlineFormClearance
	#' @param OnlineFormClearanceID The ID of the OnlineFormClearance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The OnlineFormClearanceID of the deleted OnlineFormClearance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOnlineFormClearance <- function(OnlineFormClearanceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "OnlineFormClearance", objectId = OnlineFormClearanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OnlineFormClearance
	#'
	#' This function creates an OnlineFormClearance
	#' @param fieldNames The field values to give the created OnlineFormClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created OnlineFormClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOnlineFormClearance <- function(OnlineFormID = NULL, GroupIDSecurity = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "OnlineFormClearance", body = list(DataObject = body), searchFields = append("OnlineFormClearanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OnlineFormClearance
	#'
	#' This function modifies an OnlineFormClearance
	#' @param fieldNames The field values to give the modified OnlineFormClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified OnlineFormClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOnlineFormClearance <- function(OnlineFormClearanceID, OnlineFormID = NULL, GroupIDSecurity = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "OnlineFormClearance", objectId = OnlineFormClearanceID, body = list(DataObject = body), searchFields = append("OnlineFormClearanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDataGridRows
	#'
	#' This function returns a dataframe or json object of TempDataGridRows
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDataGridRows. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDataGridRows.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDataGridRow') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of TempDataGridRows
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDataGridRows <- function(searchConditionsList = NULL, TempDataGridRowID = F, ColumnHeader = F, ControlType = F, DefaultValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Options = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "TempDataGridRow", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDataGridRow
	#'
	#' This function returns a dataframe or json object of a TempDataGridRow
	#' @param TempDataGridRowID The ID of the TempDataGridRow to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDataGridRow. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDataGridRow.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDataGridRow') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of TempDataGridRow
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDataGridRow <- function(TempDataGridRowID, ColumnHeader = F, ControlType = F, DefaultValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Options = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDataGridRowID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "TempDataGridRow", objectId = TempDataGridRowID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDataGridRow
	#'
	#' This function deletes a TempDataGridRow
	#' @param TempDataGridRowID The ID of the TempDataGridRow to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The TempDataGridRowID of the deleted TempDataGridRow.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDataGridRow <- function(TempDataGridRowID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "TempDataGridRow", objectId = TempDataGridRowID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDataGridRow
	#'
	#' This function creates a TempDataGridRow
	#' @param fieldNames The field values to give the created TempDataGridRow. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created TempDataGridRow
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDataGridRow <- function(ColumnHeader = NULL, ControlType = NULL, DefaultValue = NULL, Options = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "TempDataGridRow", body = list(DataObject = body), searchFields = append("TempDataGridRowID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDataGridRow
	#'
	#' This function modifies a TempDataGridRow
	#' @param fieldNames The field values to give the modified TempDataGridRow. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified TempDataGridRow
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDataGridRow <- function(TempDataGridRowID, ColumnHeader = NULL, ControlType = NULL, DefaultValue = NULL, Options = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "TempDataGridRow", objectId = TempDataGridRowID, body = list(DataObject = body), searchFields = append("TempDataGridRowID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TeacherOnlineForms
	#'
	#' This function returns a dataframe or json object of TeacherOnlineForms
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TeacherOnlineForms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TeacherOnlineForms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TeacherOnlineForm') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of TeacherOnlineForms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTeacherOnlineForms <- function(searchConditionsList = NULL, OnlineFormEntityID = F, OnlineFormID = F, OnlineFormStatusID = F, OnlineFormTypeID = F, EntityID = F, SchoolYearID = F, NameID = F, SectionID = F, DisplayPeriodID = F, StatusType = F, SecondaryID = F, FilterInformation = F, NoStartedFormsExist = F, NoFormsExistOfSameType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StatusTypeSortable = F, OnlineFormStatusExistsToday = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "TeacherOnlineForm", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TeacherOnlineForm
	#'
	#' This function returns a dataframe or json object of a TeacherOnlineForm
	#' @param TeacherOnlineFormID The ID of the TeacherOnlineForm to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TeacherOnlineForm. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TeacherOnlineForm.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TeacherOnlineForm') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of TeacherOnlineForm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTeacherOnlineForm <- function(TeacherOnlineFormID, OnlineFormEntityID = F, OnlineFormID = F, OnlineFormStatusID = F, OnlineFormTypeID = F, EntityID = F, SchoolYearID = F, NameID = F, SectionID = F, DisplayPeriodID = F, StatusType = F, SecondaryID = F, FilterInformation = F, NoStartedFormsExist = F, NoFormsExistOfSameType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StatusTypeSortable = F, OnlineFormStatusExistsToday = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TeacherOnlineFormID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "TeacherOnlineForm", objectId = TeacherOnlineFormID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TeacherOnlineForm
	#'
	#' This function deletes a TeacherOnlineForm
	#' @param TeacherOnlineFormID The ID of the TeacherOnlineForm to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The TeacherOnlineFormID of the deleted TeacherOnlineForm.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTeacherOnlineForm <- function(TeacherOnlineFormID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "TeacherOnlineForm", objectId = TeacherOnlineFormID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TeacherOnlineForm
	#'
	#' This function creates a TeacherOnlineForm
	#' @param fieldNames The field values to give the created TeacherOnlineForm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created TeacherOnlineForm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTeacherOnlineForm <- function(StatusType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "TeacherOnlineForm", body = list(DataObject = body), searchFields = append("TeacherOnlineFormID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TeacherOnlineForm
	#'
	#' This function modifies a TeacherOnlineForm
	#' @param fieldNames The field values to give the modified TeacherOnlineForm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified TeacherOnlineForm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTeacherOnlineForm <- function(TeacherOnlineFormID, StatusType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "TeacherOnlineForm", objectId = TeacherOnlineFormID, body = list(DataObject = body), searchFields = append("TeacherOnlineFormID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentHealthConditions
	#'
	#' This function returns a dataframe or json object of TempStudentHealthConditions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentHealthConditions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentHealthConditions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentHealthCondition') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of TempStudentHealthConditions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentHealthConditions <- function(searchConditionsList = NULL, TempStudentHealthConditionID = F, HealthConditionID = F, StudentHealthConditionID = F, StartDate = F, IsExistingStudentHealthCondition = F, IsDelete = F, OnScreenCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentHealthConditionNoteID = F, Note = F, IsExistingStudentHealthConditionNote = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "TempStudentHealthCondition", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentHealthCondition
	#'
	#' This function returns a dataframe or json object of a TempStudentHealthCondition
	#' @param TempStudentHealthConditionID The ID of the TempStudentHealthCondition to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentHealthCondition. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentHealthCondition.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentHealthCondition') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of TempStudentHealthCondition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentHealthCondition <- function(TempStudentHealthConditionID, HealthConditionID = F, StudentHealthConditionID = F, StartDate = F, IsExistingStudentHealthCondition = F, IsDelete = F, OnScreenCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentHealthConditionNoteID = F, Note = F, IsExistingStudentHealthConditionNote = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentHealthConditionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "TempStudentHealthCondition", objectId = TempStudentHealthConditionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentHealthCondition
	#'
	#' This function deletes a TempStudentHealthCondition
	#' @param TempStudentHealthConditionID The ID of the TempStudentHealthCondition to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The TempStudentHealthConditionID of the deleted TempStudentHealthCondition.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentHealthCondition <- function(TempStudentHealthConditionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "TempStudentHealthCondition", objectId = TempStudentHealthConditionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentHealthCondition
	#'
	#' This function creates a TempStudentHealthCondition
	#' @param fieldNames The field values to give the created TempStudentHealthCondition. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created TempStudentHealthCondition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentHealthCondition <- function(HealthConditionID = NULL, StudentHealthConditionID = NULL, StartDate = NULL, IsExistingStudentHealthCondition = NULL, IsDelete = NULL, OnScreenCount = NULL, StudentHealthConditionNoteID = NULL, Note = NULL, IsExistingStudentHealthConditionNote = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "TempStudentHealthCondition", body = list(DataObject = body), searchFields = append("TempStudentHealthConditionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentHealthCondition
	#'
	#' This function modifies a TempStudentHealthCondition
	#' @param fieldNames The field values to give the modified TempStudentHealthCondition. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified TempStudentHealthCondition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentHealthCondition <- function(TempStudentHealthConditionID, HealthConditionID = NULL, StudentHealthConditionID = NULL, StartDate = NULL, IsExistingStudentHealthCondition = NULL, IsDelete = NULL, OnScreenCount = NULL, StudentHealthConditionNoteID = NULL, Note = NULL, IsExistingStudentHealthConditionNote = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "TempStudentHealthCondition", objectId = TempStudentHealthConditionID, body = list(DataObject = body), searchFields = append("TempStudentHealthConditionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OnlineFormTempErrors
	#'
	#' This function returns a dataframe or json object of OnlineFormTempErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormTempErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormTempErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormTempError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of OnlineFormTempErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOnlineFormTempErrors <- function(searchConditionsList = NULL, TempErrorID = F, Description = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "TempError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OnlineFormTempError
	#'
	#' This function returns a dataframe or json object of an OnlineFormTempError
	#' @param OnlineFormTempErrorID The ID of the OnlineFormTempError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormTempError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormTempError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormTempError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of OnlineFormTempError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOnlineFormTempError <- function(OnlineFormTempErrorID, TempErrorID = F, Description = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OnlineFormTempErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "TempError", objectId = OnlineFormTempErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OnlineFormTempError
	#'
	#' This function deletes an OnlineFormTempError
	#' @param OnlineFormTempErrorID The ID of the OnlineFormTempError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The OnlineFormTempErrorID of the deleted OnlineFormTempError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOnlineFormTempError <- function(OnlineFormTempErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "TempError", objectId = OnlineFormTempErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OnlineFormTempError
	#'
	#' This function creates an OnlineFormTempError
	#' @param fieldNames The field values to give the created OnlineFormTempError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created OnlineFormTempError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOnlineFormTempError <- function(Description = NULL, Message = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "TempError", body = list(DataObject = body), searchFields = append("TempErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OnlineFormTempError
	#'
	#' This function modifies an OnlineFormTempError
	#' @param fieldNames The field values to give the modified OnlineFormTempError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified OnlineFormTempError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOnlineFormTempError <- function(TempErrorID, Description = NULL, Message = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "TempError", objectId = TempErrorID, body = list(DataObject = body), searchFields = append("TempErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStepErrors
	#'
	#' This function returns a dataframe or json object of TempStepErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStepErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStepErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStepError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of TempStepErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStepErrors <- function(searchConditionsList = NULL, TempStepErrorID = F, TempStepID = F, ErrorMessage = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "TempStepError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStepError
	#'
	#' This function returns a dataframe or json object of a TempStepError
	#' @param TempStepErrorID The ID of the TempStepError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStepError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStepError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStepError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of TempStepError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStepError <- function(TempStepErrorID, TempStepID = F, ErrorMessage = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStepErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "TempStepError", objectId = TempStepErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStepError
	#'
	#' This function deletes a TempStepError
	#' @param TempStepErrorID The ID of the TempStepError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The TempStepErrorID of the deleted TempStepError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStepError <- function(TempStepErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "TempStepError", objectId = TempStepErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStepError
	#'
	#' This function creates a TempStepError
	#' @param fieldNames The field values to give the created TempStepError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created TempStepError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStepError <- function(TempStepID = NULL, ErrorMessage = NULL, Error = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "TempStepError", body = list(DataObject = body), searchFields = append("TempStepErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStepError
	#'
	#' This function modifies a TempStepError
	#' @param fieldNames The field values to give the modified TempStepError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified TempStepError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStepError <- function(TempStepErrorID, TempStepID = NULL, ErrorMessage = NULL, Error = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "TempStepError", objectId = TempStepErrorID, body = list(DataObject = body), searchFields = append("TempStepErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OnlineFormReportableReports
	#'
	#' This function returns a dataframe or json object of OnlineFormReportableReports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormReportableReports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormReportableReports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormReportableReport') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of OnlineFormReportableReports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOnlineFormReportableReports <- function(searchConditionsList = NULL, OnlineFormReportableReportID = F, ReportID = F, OnlineFormID = F, ElementID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "OnlineFormReportableReport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OnlineFormReportableReport
	#'
	#' This function returns a dataframe or json object of an OnlineFormReportableReport
	#' @param OnlineFormReportableReportID The ID of the OnlineFormReportableReport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormReportableReport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormReportableReport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormReportableReport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of OnlineFormReportableReport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOnlineFormReportableReport <- function(OnlineFormReportableReportID, ReportID = F, OnlineFormID = F, ElementID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OnlineFormReportableReportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "OnlineFormReportableReport", objectId = OnlineFormReportableReportID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OnlineFormReportableReport
	#'
	#' This function deletes an OnlineFormReportableReport
	#' @param OnlineFormReportableReportID The ID of the OnlineFormReportableReport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The OnlineFormReportableReportID of the deleted OnlineFormReportableReport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOnlineFormReportableReport <- function(OnlineFormReportableReportID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "OnlineFormReportableReport", objectId = OnlineFormReportableReportID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OnlineFormReportableReport
	#'
	#' This function creates an OnlineFormReportableReport
	#' @param fieldNames The field values to give the created OnlineFormReportableReport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created OnlineFormReportableReport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOnlineFormReportableReport <- function(ReportID = NULL, OnlineFormID = NULL, ElementID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "OnlineFormReportableReport", body = list(DataObject = body), searchFields = append("OnlineFormReportableReportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OnlineFormReportableReport
	#'
	#' This function modifies an OnlineFormReportableReport
	#' @param fieldNames The field values to give the modified OnlineFormReportableReport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified OnlineFormReportableReport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOnlineFormReportableReport <- function(OnlineFormReportableReportID, ReportID = NULL, OnlineFormID = NULL, ElementID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "OnlineFormReportableReport", objectId = OnlineFormReportableReportID, body = list(DataObject = body), searchFields = append("OnlineFormReportableReportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OnlineFormReportableObjects
	#'
	#' This function returns a dataframe or json object of OnlineFormReportableObjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormReportableObjects. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormReportableObjects.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormReportableObject') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of OnlineFormReportableObjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOnlineFormReportableObjects <- function(searchConditionsList = NULL, OnlineFormReportableObjectID = F, ObjectID = F, OnlineFormID = F, DeleteOrder = F, ElementID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "OnlineFormReportableObject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OnlineFormReportableObject
	#'
	#' This function returns a dataframe or json object of an OnlineFormReportableObject
	#' @param OnlineFormReportableObjectID The ID of the OnlineFormReportableObject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormReportableObject. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormReportableObject.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormReportableObject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of OnlineFormReportableObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOnlineFormReportableObject <- function(OnlineFormReportableObjectID, ObjectID = F, OnlineFormID = F, DeleteOrder = F, ElementID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OnlineFormReportableObjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "OnlineFormReportableObject", objectId = OnlineFormReportableObjectID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OnlineFormReportableObject
	#'
	#' This function deletes an OnlineFormReportableObject
	#' @param OnlineFormReportableObjectID The ID of the OnlineFormReportableObject to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The OnlineFormReportableObjectID of the deleted OnlineFormReportableObject.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOnlineFormReportableObject <- function(OnlineFormReportableObjectID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "OnlineFormReportableObject", objectId = OnlineFormReportableObjectID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OnlineFormReportableObject
	#'
	#' This function creates an OnlineFormReportableObject
	#' @param fieldNames The field values to give the created OnlineFormReportableObject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A newly created OnlineFormReportableObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOnlineFormReportableObject <- function(ObjectID = NULL, OnlineFormID = NULL, DeleteOrder = NULL, ElementID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "OnlineForm", objectName = "OnlineFormReportableObject", body = list(DataObject = body), searchFields = append("OnlineFormReportableObjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OnlineFormReportableObject
	#'
	#' This function modifies an OnlineFormReportableObject
	#' @param fieldNames The field values to give the modified OnlineFormReportableObject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The modified OnlineFormReportableObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOnlineFormReportableObject <- function(OnlineFormReportableObjectID, ObjectID = NULL, OnlineFormID = NULL, DeleteOrder = NULL, ElementID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "OnlineForm", objectName = "OnlineFormReportableObject", objectId = OnlineFormReportableObjectID, body = list(DataObject = body), searchFields = append("OnlineFormReportableObjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Preschool1perStudents
	#'
	#' This function returns a dataframe or json object of Preschool1perStudents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Preschool1perStudents. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Preschool1perStudents.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Preschool1perStudent') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A list of Preschool1perStudents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPreschool1perStudents <- function(searchConditionsList = NULL, Preschool1perStudentID = F, OnlineFormStatusID = F, OnlineFormID = F, Step1Element1 = F, Step1Element2 = F, Step2Element3 = F, Step2Element4 = F, Step2Element5 = F, Step2Element7 = F, Step2Element8 = F, Step2Element9 = F, Step3Element4 = F, Step3Element8 = F, Step3Element12 = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "OnlineForm", objectName = "Preschool1perStudent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Preschool1perStudent
	#'
	#' This function returns a dataframe or json object of a Preschool1perStudent
	#' @param Preschool1perStudentID The ID of the Preschool1perStudent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Preschool1perStudent. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Preschool1perStudent.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Preschool1perStudent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return A dataframe or of Preschool1perStudent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPreschool1perStudent <- function(Preschool1perStudentID, OnlineFormStatusID = F, OnlineFormID = F, Step1Element1 = F, Step1Element2 = F, Step2Element3 = F, Step2Element4 = F, Step2Element5 = F, Step2Element7 = F, Step2Element8 = F, Step2Element9 = F, Step3Element4 = F, Step3Element8 = F, Step3Element12 = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "Preschool1perStudentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "OnlineForm", objectName = "Preschool1perStudent", objectId = Preschool1perStudentID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Preschool1perStudent
	#'
	#' This function deletes a Preschool1perStudent
	#' @param Preschool1perStudentID The ID of the Preschool1perStudent to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept OnlineForm
	#' @return The Preschool1perStudentID of the deleted Preschool1perStudent.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePreschool1perStudent <- function(Preschool1perStudentID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "OnlineForm", objectName = "Preschool1perStudent", objectId = Preschool1perStudentID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
