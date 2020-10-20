
	#' List AssetLocationTransactions
	#'
	#' This function returns a dataframe or json object of AssetLocationTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssetLocationTransactions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssetLocationTransactions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssetLocationTransaction') to get more field paths.
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
	#' @concept Asset
	#' @return A list of AssetLocationTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssetLocationTransactions <- function(searchConditionsList = NULL, AssetLocationTransactionID = F, AssetID = F, NameID = F, CheckInTime = F, BuildingID = F, RoomID = F, WarehouseID = F, WarehouseLocationID = F, LocationComment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Location = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "AssetLocationTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssetLocationTransaction
	#'
	#' This function returns a dataframe or json object of an AssetLocationTransaction
	#' @param AssetLocationTransactionID The ID of the AssetLocationTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssetLocationTransaction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssetLocationTransaction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssetLocationTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of AssetLocationTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssetLocationTransaction <- function(AssetLocationTransactionID, AssetID = F, NameID = F, CheckInTime = F, BuildingID = F, RoomID = F, WarehouseID = F, WarehouseLocationID = F, LocationComment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Location = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssetLocationTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "AssetLocationTransaction", objectId = AssetLocationTransactionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssetLocationTransaction
	#'
	#' This function deletes an AssetLocationTransaction
	#' @param AssetLocationTransactionID The ID of the AssetLocationTransaction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The AssetLocationTransactionID of the deleted AssetLocationTransaction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssetLocationTransaction <- function(AssetLocationTransactionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "AssetLocationTransaction", objectId = AssetLocationTransactionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssetLocationTransaction
	#'
	#' This function creates an AssetLocationTransaction
	#' @param fieldNames The field values to give the created AssetLocationTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created AssetLocationTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssetLocationTransaction <- function(AssetID = NULL, NameID = NULL, CheckInTime = NULL, BuildingID = NULL, RoomID = NULL, WarehouseID = NULL, WarehouseLocationID = NULL, LocationComment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "AssetLocationTransaction", body = list(DataObject = body), searchFields = append("AssetLocationTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssetLocationTransaction
	#'
	#' This function modifies an AssetLocationTransaction
	#' @param fieldNames The field values to give the modified AssetLocationTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified AssetLocationTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssetLocationTransaction <- function(AssetLocationTransactionID, AssetID = NULL, NameID = NULL, CheckInTime = NULL, BuildingID = NULL, RoomID = NULL, WarehouseID = NULL, WarehouseLocationID = NULL, LocationComment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "AssetLocationTransaction", objectId = AssetLocationTransactionID, body = list(DataObject = body), searchFields = append("AssetLocationTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Assets
	#'
	#' This function returns a dataframe or json object of Assets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Assets. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Assets.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Asset') to get more field paths.
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
	#' @concept Asset
	#' @return A list of Assets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssets <- function(searchConditionsList = NULL, AssetID = F, ItemID = F, TagNumber = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ModelNumber = F, Manufacturer = F, Warranty = F, WarrantyExpirationDate = F, AttachmentCount = F, VendorID = F, PreliminaryAssetID = F, InvoiceID = F, InvoiceDetailID = F, PurchaseOrderID = F, PurchaseOrderDetailID = F, ReceivingID = F, InvoiceNumber = F, PurchaseOrderNumber = F, SerialNumber = F, IsDepreciable = F, DepreciationYears = F, SalvageValue = F, CheckNumber = F, DateInService = F, DateOutOfService = F, FiscalYearCurrentValue = F, FiscalYearAdditionAmount = F, FiscalYearDisposalAmount = F, FiscalYearDepreciationAmount = F, OriginalAdditionAmount = F, MediaIDPhoto = F, PreviousCurrentValue = F, PreviousAdditionAmount = F, PreviousDisposalAmount = F, PreviousDepreciationAmount = F, AssetNumber = F, OriginAmount = F, HasDisposal = F, IsActiveInFiscalYear = F, DistrictID = F, AssetClassID = F, SourceComment = F, FiscalYearAccumulatedAdditionAmount = F, FiscalYearAccumulatedDisposalAmount = F, FiscalYearAccumulatedDepreciationAmount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "Asset", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Asset
	#'
	#' This function returns a dataframe or json object of an Asset
	#' @param AssetID The ID of the Asset to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Asset. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Asset.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Asset') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of Asset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAsset <- function(AssetID, ItemID = F, TagNumber = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ModelNumber = F, Manufacturer = F, Warranty = F, WarrantyExpirationDate = F, AttachmentCount = F, VendorID = F, PreliminaryAssetID = F, InvoiceID = F, InvoiceDetailID = F, PurchaseOrderID = F, PurchaseOrderDetailID = F, ReceivingID = F, InvoiceNumber = F, PurchaseOrderNumber = F, SerialNumber = F, IsDepreciable = F, DepreciationYears = F, SalvageValue = F, CheckNumber = F, DateInService = F, DateOutOfService = F, FiscalYearCurrentValue = F, FiscalYearAdditionAmount = F, FiscalYearDisposalAmount = F, FiscalYearDepreciationAmount = F, OriginalAdditionAmount = F, MediaIDPhoto = F, PreviousCurrentValue = F, PreviousAdditionAmount = F, PreviousDisposalAmount = F, PreviousDepreciationAmount = F, AssetNumber = F, OriginAmount = F, HasDisposal = F, IsActiveInFiscalYear = F, DistrictID = F, AssetClassID = F, SourceComment = F, FiscalYearAccumulatedAdditionAmount = F, FiscalYearAccumulatedDisposalAmount = F, FiscalYearAccumulatedDepreciationAmount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "Asset", objectId = AssetID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Asset
	#'
	#' This function deletes an Asset
	#' @param AssetID The ID of the Asset to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The AssetID of the deleted Asset.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAsset <- function(AssetID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "Asset", objectId = AssetID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Asset
	#'
	#' This function creates an Asset
	#' @param fieldNames The field values to give the created Asset. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created Asset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAsset <- function(ItemID = NULL, TagNumber = NULL, Description = NULL, ModelNumber = NULL, Manufacturer = NULL, Warranty = NULL, WarrantyExpirationDate = NULL, VendorID = NULL, PreliminaryAssetID = NULL, InvoiceID = NULL, InvoiceDetailID = NULL, PurchaseOrderID = NULL, PurchaseOrderDetailID = NULL, ReceivingID = NULL, InvoiceNumber = NULL, PurchaseOrderNumber = NULL, SerialNumber = NULL, IsDepreciable = NULL, DepreciationYears = NULL, SalvageValue = NULL, CheckNumber = NULL, MediaIDPhoto = NULL, AssetNumber = NULL, OriginAmount = NULL, DistrictID = NULL, AssetClassID = NULL, SourceComment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "Asset", body = list(DataObject = body), searchFields = append("AssetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Asset
	#'
	#' This function modifies an Asset
	#' @param fieldNames The field values to give the modified Asset. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified Asset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAsset <- function(AssetID, ItemID = NULL, TagNumber = NULL, Description = NULL, ModelNumber = NULL, Manufacturer = NULL, Warranty = NULL, WarrantyExpirationDate = NULL, VendorID = NULL, PreliminaryAssetID = NULL, InvoiceID = NULL, InvoiceDetailID = NULL, PurchaseOrderID = NULL, PurchaseOrderDetailID = NULL, ReceivingID = NULL, InvoiceNumber = NULL, PurchaseOrderNumber = NULL, SerialNumber = NULL, IsDepreciable = NULL, DepreciationYears = NULL, SalvageValue = NULL, CheckNumber = NULL, MediaIDPhoto = NULL, AssetNumber = NULL, OriginAmount = NULL, DistrictID = NULL, AssetClassID = NULL, SourceComment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "Asset", objectId = AssetID, body = list(DataObject = body), searchFields = append("AssetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssetItems
	#'
	#' This function returns a dataframe or json object of AssetItems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssetItems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssetItems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssetItem') to get more field paths.
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
	#' @concept Asset
	#' @return A list of AssetItems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssetItems <- function(searchConditionsList = NULL, ItemID = F, Code = F, Description = F, ItemCategoryID = F, UnitOfMeasureIDStock = F, MediaIDPhoto = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalAssetCount = F, FiscalYearCurrentValue = F, FiscalYearAdditions = F, FiscalYearDisposals = F, FiscalYearDepreciation = F, PreviousCurrentValue = F, PreviousAdditions = F, PreviousDisposals = F, PreviousDepreciation = F, IsDepreciable = F, DepreciationYears = F, AccountIDConsumableItem = F, ReorderPoint = F, ReorderQuantity = F, ReorderPointDifference = F, StockOnHand = F, UnfulfilledQuantity = F, AvailableQuantity = F, BackOrderedQuantity = F, IsBackOrdered = F, AverageUnitCost = F, Comment = F, IsActive = F, FiscalYearAccumulatedAdditions = F, FiscalYearAccumulatedDisposals = F, FiscalYearAccumulatedDepreciation = F, AllowBackorders = F, HasMaximumWarehouseRequestQuantity = F, MaximumWarehouseRequestQuantity = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "Item", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssetItem
	#'
	#' This function returns a dataframe or json object of an AssetItem
	#' @param AssetItemID The ID of the AssetItem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssetItem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssetItem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssetItem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of AssetItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssetItem <- function(AssetItemID, ItemID = F, Code = F, Description = F, ItemCategoryID = F, UnitOfMeasureIDStock = F, MediaIDPhoto = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalAssetCount = F, FiscalYearCurrentValue = F, FiscalYearAdditions = F, FiscalYearDisposals = F, FiscalYearDepreciation = F, PreviousCurrentValue = F, PreviousAdditions = F, PreviousDisposals = F, PreviousDepreciation = F, IsDepreciable = F, DepreciationYears = F, AccountIDConsumableItem = F, ReorderPoint = F, ReorderQuantity = F, ReorderPointDifference = F, StockOnHand = F, UnfulfilledQuantity = F, AvailableQuantity = F, BackOrderedQuantity = F, IsBackOrdered = F, AverageUnitCost = F, Comment = F, IsActive = F, FiscalYearAccumulatedAdditions = F, FiscalYearAccumulatedDisposals = F, FiscalYearAccumulatedDepreciation = F, AllowBackorders = F, HasMaximumWarehouseRequestQuantity = F, MaximumWarehouseRequestQuantity = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssetItemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "Item", objectId = AssetItemID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssetItem
	#'
	#' This function deletes an AssetItem
	#' @param AssetItemID The ID of the AssetItem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The AssetItemID of the deleted AssetItem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssetItem <- function(AssetItemID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "Item", objectId = AssetItemID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssetItem
	#'
	#' This function creates an AssetItem
	#' @param fieldNames The field values to give the created AssetItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created AssetItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssetItem <- function(Code = NULL, Description = NULL, ItemCategoryID = NULL, UnitOfMeasureIDStock = NULL, MediaIDPhoto = NULL, IsDepreciable = NULL, DepreciationYears = NULL, AccountIDConsumableItem = NULL, ReorderPoint = NULL, ReorderQuantity = NULL, Comment = NULL, IsActive = NULL, AllowBackorders = NULL, HasMaximumWarehouseRequestQuantity = NULL, MaximumWarehouseRequestQuantity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "Item", body = list(DataObject = body), searchFields = append("ItemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssetItem
	#'
	#' This function modifies an AssetItem
	#' @param fieldNames The field values to give the modified AssetItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified AssetItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssetItem <- function(ItemID, Code = NULL, Description = NULL, ItemCategoryID = NULL, UnitOfMeasureIDStock = NULL, MediaIDPhoto = NULL, IsDepreciable = NULL, DepreciationYears = NULL, AccountIDConsumableItem = NULL, ReorderPoint = NULL, ReorderQuantity = NULL, Comment = NULL, IsActive = NULL, AllowBackorders = NULL, HasMaximumWarehouseRequestQuantity = NULL, MaximumWarehouseRequestQuantity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "Item", objectId = ItemID, body = list(DataObject = body), searchFields = append("ItemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ItemCategories
	#'
	#' This function returns a dataframe or json object of ItemCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ItemCategories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ItemCategories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ItemCategory') to get more field paths.
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
	#' @concept Asset
	#' @return A list of ItemCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listItemCategories <- function(searchConditionsList = NULL, ItemCategoryID = F, Code = F, Description = F, IsConsumable = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalItemCount = F, TotalAssetCount = F, FiscalYearCurrentValue = F, FiscalYearAdditions = F, FiscalYearDisposals = F, FiscalYearDepreciation = F, PreviousCurrentValue = F, PreviousAdditions = F, PreviousDisposals = F, PreviousDepreciation = F, FiscalYearAccumulatedAdditions = F, FiscalYearAccumulatedDisposals = F, FiscalYearAccumulatedDepreciation = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "ItemCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ItemCategory
	#'
	#' This function returns a dataframe or json object of an ItemCategory
	#' @param ItemCategoryID The ID of the ItemCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ItemCategory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ItemCategory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ItemCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of ItemCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getItemCategory <- function(ItemCategoryID, Code = F, Description = F, IsConsumable = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalItemCount = F, TotalAssetCount = F, FiscalYearCurrentValue = F, FiscalYearAdditions = F, FiscalYearDisposals = F, FiscalYearDepreciation = F, PreviousCurrentValue = F, PreviousAdditions = F, PreviousDisposals = F, PreviousDepreciation = F, FiscalYearAccumulatedAdditions = F, FiscalYearAccumulatedDisposals = F, FiscalYearAccumulatedDepreciation = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ItemCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "ItemCategory", objectId = ItemCategoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ItemCategory
	#'
	#' This function deletes an ItemCategory
	#' @param ItemCategoryID The ID of the ItemCategory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The ItemCategoryID of the deleted ItemCategory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteItemCategory <- function(ItemCategoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "ItemCategory", objectId = ItemCategoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ItemCategory
	#'
	#' This function creates an ItemCategory
	#' @param fieldNames The field values to give the created ItemCategory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created ItemCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createItemCategory <- function(Code = NULL, Description = NULL, IsConsumable = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "ItemCategory", body = list(DataObject = body), searchFields = append("ItemCategoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ItemCategory
	#'
	#' This function modifies an ItemCategory
	#' @param fieldNames The field values to give the modified ItemCategory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified ItemCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyItemCategory <- function(ItemCategoryID, Code = NULL, Description = NULL, IsConsumable = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "ItemCategory", objectId = ItemCategoryID, body = list(DataObject = body), searchFields = append("ItemCategoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PreliminaryAssets
	#'
	#' This function returns a dataframe or json object of PreliminaryAssets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PreliminaryAssets. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PreliminaryAssets.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PreliminaryAsset') to get more field paths.
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
	#' @concept Asset
	#' @return A list of PreliminaryAssets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPreliminaryAssets <- function(searchConditionsList = NULL, PreliminaryAssetID = F, DistrictID = F, InvoiceDetailID = F, PurchaseOrderDetailID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalAssetsCreated = F, TotalAssetsExpected = F, TotalAssetsNotCreated = F, IsComplete = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "PreliminaryAsset", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PreliminaryAsset
	#'
	#' This function returns a dataframe or json object of a PreliminaryAsset
	#' @param PreliminaryAssetID The ID of the PreliminaryAsset to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PreliminaryAsset. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PreliminaryAsset.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PreliminaryAsset') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of PreliminaryAsset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPreliminaryAsset <- function(PreliminaryAssetID, DistrictID = F, InvoiceDetailID = F, PurchaseOrderDetailID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalAssetsCreated = F, TotalAssetsExpected = F, TotalAssetsNotCreated = F, IsComplete = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PreliminaryAssetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "PreliminaryAsset", objectId = PreliminaryAssetID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PreliminaryAsset
	#'
	#' This function deletes a PreliminaryAsset
	#' @param PreliminaryAssetID The ID of the PreliminaryAsset to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The PreliminaryAssetID of the deleted PreliminaryAsset.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePreliminaryAsset <- function(PreliminaryAssetID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "PreliminaryAsset", objectId = PreliminaryAssetID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PreliminaryAsset
	#'
	#' This function creates a PreliminaryAsset
	#' @param fieldNames The field values to give the created PreliminaryAsset. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created PreliminaryAsset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPreliminaryAsset <- function(DistrictID = NULL, InvoiceDetailID = NULL, PurchaseOrderDetailID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "PreliminaryAsset", body = list(DataObject = body), searchFields = append("PreliminaryAssetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PreliminaryAsset
	#'
	#' This function modifies a PreliminaryAsset
	#' @param fieldNames The field values to give the modified PreliminaryAsset. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified PreliminaryAsset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPreliminaryAsset <- function(PreliminaryAssetID, DistrictID = NULL, InvoiceDetailID = NULL, PurchaseOrderDetailID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "PreliminaryAsset", objectId = PreliminaryAssetID, body = list(DataObject = body), searchFields = append("PreliminaryAssetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssetConfigDistricts
	#'
	#' This function returns a dataframe or json object of AssetConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssetConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssetConfigDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssetConfigDistrict') to get more field paths.
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
	#' @concept Asset
	#' @return A list of AssetConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssetConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AccountDefaultSource = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssetConfigDistrict
	#'
	#' This function returns a dataframe or json object of an AssetConfigDistrict
	#' @param AssetConfigDistrictID The ID of the AssetConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssetConfigDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssetConfigDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssetConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of AssetConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssetConfigDistrict <- function(AssetConfigDistrictID, ConfigDistrictID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AccountDefaultSource = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssetConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "ConfigDistrict", objectId = AssetConfigDistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssetConfigDistrict
	#'
	#' This function deletes an AssetConfigDistrict
	#' @param AssetConfigDistrictID The ID of the AssetConfigDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The AssetConfigDistrictID of the deleted AssetConfigDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssetConfigDistrict <- function(AssetConfigDistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "ConfigDistrict", objectId = AssetConfigDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssetConfigDistrict
	#'
	#' This function creates an AssetConfigDistrict
	#' @param fieldNames The field values to give the created AssetConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created AssetConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssetConfigDistrict <- function(DistrictID = NULL, AccountDefaultSource = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "ConfigDistrict", body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssetConfigDistrict
	#'
	#' This function modifies an AssetConfigDistrict
	#' @param fieldNames The field values to give the modified AssetConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified AssetConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssetConfigDistrict <- function(ConfigDistrictID, DistrictID = NULL, AccountDefaultSource = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "ConfigDistrict", objectId = ConfigDistrictID, body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NextAssetTagNumbers
	#'
	#' This function returns a dataframe or json object of NextAssetTagNumbers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextAssetTagNumbers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextAssetTagNumbers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextAssetTagNumber') to get more field paths.
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
	#' @concept Asset
	#' @return A list of NextAssetTagNumbers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNextAssetTagNumbers <- function(searchConditionsList = NULL, NextAssetTagNumberID = F, AssetTagNumber = F, ConfigDistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AssetNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "NextAssetTagNumber", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NextAssetTagNumber
	#'
	#' This function returns a dataframe or json object of a NextAssetTagNumber
	#' @param NextAssetTagNumberID The ID of the NextAssetTagNumber to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextAssetTagNumber. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextAssetTagNumber.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextAssetTagNumber') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of NextAssetTagNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNextAssetTagNumber <- function(NextAssetTagNumberID, AssetTagNumber = F, ConfigDistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AssetNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NextAssetTagNumberID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "NextAssetTagNumber", objectId = NextAssetTagNumberID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NextAssetTagNumber
	#'
	#' This function deletes a NextAssetTagNumber
	#' @param NextAssetTagNumberID The ID of the NextAssetTagNumber to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The NextAssetTagNumberID of the deleted NextAssetTagNumber.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNextAssetTagNumber <- function(NextAssetTagNumberID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "NextAssetTagNumber", objectId = NextAssetTagNumberID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NextAssetTagNumber
	#'
	#' This function creates a NextAssetTagNumber
	#' @param fieldNames The field values to give the created NextAssetTagNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created NextAssetTagNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNextAssetTagNumber <- function(AssetTagNumber = NULL, ConfigDistrictID = NULL, AssetNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "NextAssetTagNumber", body = list(DataObject = body), searchFields = append("NextAssetTagNumberID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NextAssetTagNumber
	#'
	#' This function modifies a NextAssetTagNumber
	#' @param fieldNames The field values to give the modified NextAssetTagNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified NextAssetTagNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNextAssetTagNumber <- function(NextAssetTagNumberID, AssetTagNumber = NULL, ConfigDistrictID = NULL, AssetNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "NextAssetTagNumber", objectId = NextAssetTagNumberID, body = list(DataObject = body), searchFields = append("NextAssetTagNumberID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAssets
	#'
	#' This function returns a dataframe or json object of TempAssets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAssets. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAssets.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAsset') to get more field paths.
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
	#' @concept Asset
	#' @return A list of TempAssets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAssets <- function(searchConditionsList = NULL, TempAssetID = F, AssetID = F, IsCheckBoxReadOnly = F, CreateOrUpdateAsset = F, PreliminaryAssetID = F, ItemID = F, Description = F, TagNumber = F, PurchaseOrderID = F, PurchaseOrderNumber = F, PurchaseOrderDetailID = F, InvoiceID = F, InvoiceNumber = F, InvoiceDetailID = F, DetailDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, VendorID = F, IsDepreciable = F, DepreciationYears = F, SalvageValue = F, AssetNumber = F, DistrictID = F, AssetClassID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "TempAsset", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAsset
	#'
	#' This function returns a dataframe or json object of a TempAsset
	#' @param TempAssetID The ID of the TempAsset to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAsset. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAsset.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAsset') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of TempAsset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAsset <- function(TempAssetID, AssetID = F, IsCheckBoxReadOnly = F, CreateOrUpdateAsset = F, PreliminaryAssetID = F, ItemID = F, Description = F, TagNumber = F, PurchaseOrderID = F, PurchaseOrderNumber = F, PurchaseOrderDetailID = F, InvoiceID = F, InvoiceNumber = F, InvoiceDetailID = F, DetailDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, VendorID = F, IsDepreciable = F, DepreciationYears = F, SalvageValue = F, AssetNumber = F, DistrictID = F, AssetClassID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAssetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "TempAsset", objectId = TempAssetID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAsset
	#'
	#' This function deletes a TempAsset
	#' @param TempAssetID The ID of the TempAsset to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The TempAssetID of the deleted TempAsset.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAsset <- function(TempAssetID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "TempAsset", objectId = TempAssetID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAsset
	#'
	#' This function creates a TempAsset
	#' @param fieldNames The field values to give the created TempAsset. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created TempAsset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAsset <- function(AssetID = NULL, IsCheckBoxReadOnly = NULL, CreateOrUpdateAsset = NULL, PreliminaryAssetID = NULL, ItemID = NULL, Description = NULL, TagNumber = NULL, PurchaseOrderID = NULL, PurchaseOrderNumber = NULL, PurchaseOrderDetailID = NULL, InvoiceID = NULL, InvoiceNumber = NULL, InvoiceDetailID = NULL, DetailDescription = NULL, VendorID = NULL, IsDepreciable = NULL, DepreciationYears = NULL, SalvageValue = NULL, AssetNumber = NULL, DistrictID = NULL, AssetClassID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "TempAsset", body = list(DataObject = body), searchFields = append("TempAssetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAsset
	#'
	#' This function modifies a TempAsset
	#' @param fieldNames The field values to give the modified TempAsset. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified TempAsset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAsset <- function(TempAssetID, AssetID = NULL, IsCheckBoxReadOnly = NULL, CreateOrUpdateAsset = NULL, PreliminaryAssetID = NULL, ItemID = NULL, Description = NULL, TagNumber = NULL, PurchaseOrderID = NULL, PurchaseOrderNumber = NULL, PurchaseOrderDetailID = NULL, InvoiceID = NULL, InvoiceNumber = NULL, InvoiceDetailID = NULL, DetailDescription = NULL, VendorID = NULL, IsDepreciable = NULL, DepreciationYears = NULL, SalvageValue = NULL, AssetNumber = NULL, DistrictID = NULL, AssetClassID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "TempAsset", objectId = TempAssetID, body = list(DataObject = body), searchFields = append("TempAssetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DepreciationAccountings
	#'
	#' This function returns a dataframe or json object of DepreciationAccountings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DepreciationAccountings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DepreciationAccountings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DepreciationAccounting') to get more field paths.
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
	#' @concept Asset
	#' @return A list of DepreciationAccountings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDepreciationAccountings <- function(searchConditionsList = NULL, DepreciationAccountingID = F, DepreciationID = F, AccountIDDepreciationExpense = F, AccountIDAccumulatedDepreciation = F, BaseCurrencyAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "DepreciationAccounting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DepreciationAccounting
	#'
	#' This function returns a dataframe or json object of a DepreciationAccounting
	#' @param DepreciationAccountingID The ID of the DepreciationAccounting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DepreciationAccounting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DepreciationAccounting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DepreciationAccounting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of DepreciationAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDepreciationAccounting <- function(DepreciationAccountingID, DepreciationID = F, AccountIDDepreciationExpense = F, AccountIDAccumulatedDepreciation = F, BaseCurrencyAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DepreciationAccountingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "DepreciationAccounting", objectId = DepreciationAccountingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DepreciationAccounting
	#'
	#' This function deletes a DepreciationAccounting
	#' @param DepreciationAccountingID The ID of the DepreciationAccounting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The DepreciationAccountingID of the deleted DepreciationAccounting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDepreciationAccounting <- function(DepreciationAccountingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "DepreciationAccounting", objectId = DepreciationAccountingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DepreciationAccounting
	#'
	#' This function creates a DepreciationAccounting
	#' @param fieldNames The field values to give the created DepreciationAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created DepreciationAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDepreciationAccounting <- function(DepreciationID = NULL, AccountIDDepreciationExpense = NULL, AccountIDAccumulatedDepreciation = NULL, BaseCurrencyAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "DepreciationAccounting", body = list(DataObject = body), searchFields = append("DepreciationAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DepreciationAccounting
	#'
	#' This function modifies a DepreciationAccounting
	#' @param fieldNames The field values to give the modified DepreciationAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified DepreciationAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDepreciationAccounting <- function(DepreciationAccountingID, DepreciationID = NULL, AccountIDDepreciationExpense = NULL, AccountIDAccumulatedDepreciation = NULL, BaseCurrencyAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "DepreciationAccounting", objectId = DepreciationAccountingID, body = list(DataObject = body), searchFields = append("DepreciationAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Depreciations
	#'
	#' This function returns a dataframe or json object of Depreciations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Depreciations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Depreciations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Depreciation') to get more field paths.
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
	#' @concept Asset
	#' @return A list of Depreciations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDepreciations <- function(searchConditionsList = NULL, DepreciationID = F, AssetID = F, Status = F, Date = F, BaseCurrencyAmount = F, AccountingUpdateID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "Depreciation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Depreciation
	#'
	#' This function returns a dataframe or json object of a Depreciation
	#' @param DepreciationID The ID of the Depreciation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Depreciation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Depreciation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Depreciation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of Depreciation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDepreciation <- function(DepreciationID, AssetID = F, Status = F, Date = F, BaseCurrencyAmount = F, AccountingUpdateID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DepreciationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "Depreciation", objectId = DepreciationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Depreciation
	#'
	#' This function deletes a Depreciation
	#' @param DepreciationID The ID of the Depreciation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The DepreciationID of the deleted Depreciation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDepreciation <- function(DepreciationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "Depreciation", objectId = DepreciationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Depreciation
	#'
	#' This function creates a Depreciation
	#' @param fieldNames The field values to give the created Depreciation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created Depreciation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDepreciation <- function(AssetID = NULL, Status = NULL, Date = NULL, BaseCurrencyAmount = NULL, AccountingUpdateID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "Depreciation", body = list(DataObject = body), searchFields = append("DepreciationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Depreciation
	#'
	#' This function modifies a Depreciation
	#' @param fieldNames The field values to give the modified Depreciation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified Depreciation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDepreciation <- function(DepreciationID, AssetID = NULL, Status = NULL, Date = NULL, BaseCurrencyAmount = NULL, AccountingUpdateID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "Depreciation", objectId = DepreciationID, body = list(DataObject = body), searchFields = append("DepreciationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AdditionDisposalAccountings
	#'
	#' This function returns a dataframe or json object of AdditionDisposalAccountings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AdditionDisposalAccountings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AdditionDisposalAccountings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AdditionDisposalAccounting') to get more field paths.
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
	#' @concept Asset
	#' @return A list of AdditionDisposalAccountings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAdditionDisposalAccountings <- function(searchConditionsList = NULL, AdditionDisposalAccountingID = F, AdditionDisposalID = F, AccountIDAsset = F, AccountIDSource = F, BaseCurrencyAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "AdditionDisposalAccounting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AdditionDisposalAccounting
	#'
	#' This function returns a dataframe or json object of an AdditionDisposalAccounting
	#' @param AdditionDisposalAccountingID The ID of the AdditionDisposalAccounting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AdditionDisposalAccounting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AdditionDisposalAccounting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AdditionDisposalAccounting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of AdditionDisposalAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAdditionDisposalAccounting <- function(AdditionDisposalAccountingID, AdditionDisposalID = F, AccountIDAsset = F, AccountIDSource = F, BaseCurrencyAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AdditionDisposalAccountingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "AdditionDisposalAccounting", objectId = AdditionDisposalAccountingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AdditionDisposalAccounting
	#'
	#' This function deletes an AdditionDisposalAccounting
	#' @param AdditionDisposalAccountingID The ID of the AdditionDisposalAccounting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The AdditionDisposalAccountingID of the deleted AdditionDisposalAccounting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAdditionDisposalAccounting <- function(AdditionDisposalAccountingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "AdditionDisposalAccounting", objectId = AdditionDisposalAccountingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AdditionDisposalAccounting
	#'
	#' This function creates an AdditionDisposalAccounting
	#' @param fieldNames The field values to give the created AdditionDisposalAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created AdditionDisposalAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAdditionDisposalAccounting <- function(AdditionDisposalID = NULL, AccountIDAsset = NULL, AccountIDSource = NULL, BaseCurrencyAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "AdditionDisposalAccounting", body = list(DataObject = body), searchFields = append("AdditionDisposalAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AdditionDisposalAccounting
	#'
	#' This function modifies an AdditionDisposalAccounting
	#' @param fieldNames The field values to give the modified AdditionDisposalAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified AdditionDisposalAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAdditionDisposalAccounting <- function(AdditionDisposalAccountingID, AdditionDisposalID = NULL, AccountIDAsset = NULL, AccountIDSource = NULL, BaseCurrencyAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "AdditionDisposalAccounting", objectId = AdditionDisposalAccountingID, body = list(DataObject = body), searchFields = append("AdditionDisposalAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AdditionDisposals
	#'
	#' This function returns a dataframe or json object of AdditionDisposals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AdditionDisposals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AdditionDisposals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AdditionDisposal') to get more field paths.
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
	#' @concept Asset
	#' @return A list of AdditionDisposals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAdditionDisposals <- function(searchConditionsList = NULL, AdditionDisposalID = F, AssetID = F, Type = F, Status = F, Date = F, BaseCurrencyAmount = F, AccountingUpdateID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "AdditionDisposal", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AdditionDisposal
	#'
	#' This function returns a dataframe or json object of an AdditionDisposal
	#' @param AdditionDisposalID The ID of the AdditionDisposal to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AdditionDisposal. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AdditionDisposal.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AdditionDisposal') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of AdditionDisposal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAdditionDisposal <- function(AdditionDisposalID, AssetID = F, Type = F, Status = F, Date = F, BaseCurrencyAmount = F, AccountingUpdateID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AdditionDisposalID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "AdditionDisposal", objectId = AdditionDisposalID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AdditionDisposal
	#'
	#' This function deletes an AdditionDisposal
	#' @param AdditionDisposalID The ID of the AdditionDisposal to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The AdditionDisposalID of the deleted AdditionDisposal.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAdditionDisposal <- function(AdditionDisposalID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "AdditionDisposal", objectId = AdditionDisposalID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AdditionDisposal
	#'
	#' This function creates an AdditionDisposal
	#' @param fieldNames The field values to give the created AdditionDisposal. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created AdditionDisposal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAdditionDisposal <- function(AssetID = NULL, Type = NULL, Status = NULL, Date = NULL, BaseCurrencyAmount = NULL, AccountingUpdateID = NULL, Comment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "AdditionDisposal", body = list(DataObject = body), searchFields = append("AdditionDisposalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AdditionDisposal
	#'
	#' This function modifies an AdditionDisposal
	#' @param fieldNames The field values to give the modified AdditionDisposal. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified AdditionDisposal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAdditionDisposal <- function(AdditionDisposalID, AssetID = NULL, Type = NULL, Status = NULL, Date = NULL, BaseCurrencyAmount = NULL, AccountingUpdateID = NULL, Comment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "AdditionDisposal", objectId = AdditionDisposalID, body = list(DataObject = body), searchFields = append("AdditionDisposalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OriginAccountings
	#'
	#' This function returns a dataframe or json object of OriginAccountings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OriginAccountings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OriginAccountings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OriginAccounting') to get more field paths.
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
	#' @concept Asset
	#' @return A list of OriginAccountings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOriginAccountings <- function(searchConditionsList = NULL, OriginAccountingID = F, AssetID = F, AccountID = F, DistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "OriginAccounting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OriginAccounting
	#'
	#' This function returns a dataframe or json object of an OriginAccounting
	#' @param OriginAccountingID The ID of the OriginAccounting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OriginAccounting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OriginAccounting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OriginAccounting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of OriginAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOriginAccounting <- function(OriginAccountingID, AssetID = F, AccountID = F, DistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OriginAccountingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "OriginAccounting", objectId = OriginAccountingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OriginAccounting
	#'
	#' This function deletes an OriginAccounting
	#' @param OriginAccountingID The ID of the OriginAccounting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The OriginAccountingID of the deleted OriginAccounting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOriginAccounting <- function(OriginAccountingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "OriginAccounting", objectId = OriginAccountingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OriginAccounting
	#'
	#' This function creates an OriginAccounting
	#' @param fieldNames The field values to give the created OriginAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created OriginAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOriginAccounting <- function(AssetID = NULL, AccountID = NULL, DistributionPercent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "OriginAccounting", body = list(DataObject = body), searchFields = append("OriginAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OriginAccounting
	#'
	#' This function modifies an OriginAccounting
	#' @param fieldNames The field values to give the modified OriginAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified OriginAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOriginAccounting <- function(OriginAccountingID, AssetID = NULL, AccountID = NULL, DistributionPercent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "OriginAccounting", objectId = OriginAccountingID, body = list(DataObject = body), searchFields = append("OriginAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ItemCategoryAccounts
	#'
	#' This function returns a dataframe or json object of ItemCategoryAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ItemCategoryAccounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ItemCategoryAccounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ItemCategoryAccount') to get more field paths.
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
	#' @concept Asset
	#' @return A list of ItemCategoryAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listItemCategoryAccounts <- function(searchConditionsList = NULL, ItemCategoryAccountID = F, ItemCategoryID = F, AccountID = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "ItemCategoryAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ItemCategoryAccount
	#'
	#' This function returns a dataframe or json object of an ItemCategoryAccount
	#' @param ItemCategoryAccountID The ID of the ItemCategoryAccount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ItemCategoryAccount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ItemCategoryAccount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ItemCategoryAccount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of ItemCategoryAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getItemCategoryAccount <- function(ItemCategoryAccountID, ItemCategoryID = F, AccountID = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ItemCategoryAccountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "ItemCategoryAccount", objectId = ItemCategoryAccountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ItemCategoryAccount
	#'
	#' This function deletes an ItemCategoryAccount
	#' @param ItemCategoryAccountID The ID of the ItemCategoryAccount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The ItemCategoryAccountID of the deleted ItemCategoryAccount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteItemCategoryAccount <- function(ItemCategoryAccountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "ItemCategoryAccount", objectId = ItemCategoryAccountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ItemCategoryAccount
	#'
	#' This function creates an ItemCategoryAccount
	#' @param fieldNames The field values to give the created ItemCategoryAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created ItemCategoryAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createItemCategoryAccount <- function(ItemCategoryID = NULL, AccountID = NULL, Type = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "ItemCategoryAccount", body = list(DataObject = body), searchFields = append("ItemCategoryAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ItemCategoryAccount
	#'
	#' This function modifies an ItemCategoryAccount
	#' @param fieldNames The field values to give the modified ItemCategoryAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified ItemCategoryAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyItemCategoryAccount <- function(ItemCategoryAccountID, ItemCategoryID = NULL, AccountID = NULL, Type = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "ItemCategoryAccount", objectId = ItemCategoryAccountID, body = list(DataObject = body), searchFields = append("ItemCategoryAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDepreciations
	#'
	#' This function returns a dataframe or json object of TempDepreciations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDepreciations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDepreciations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDepreciation') to get more field paths.
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
	#' @concept Asset
	#' @return A list of TempDepreciations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDepreciations <- function(searchConditionsList = NULL, TempDepreciationID = F, AssetID = F, Date = F, BaseCurrencyAmount = F, AssetTagNumber = F, AssetNumber = F, Item = F, AccountIDDepreciationExpense = F, AccountIDAccumulatedDepreciation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AssetDescription = F, IsError = F, ErrorMessage = F, AccountSummaryAccumulatedDepreciation = F, AccountSummaryDepreciationExpense = F, AccountSource = F, HasDefaultAccumulatedDepreciationAccount = F, HasDefaultDepreciationExpenseAccount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "TempDepreciation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDepreciation
	#'
	#' This function returns a dataframe or json object of a TempDepreciation
	#' @param TempDepreciationID The ID of the TempDepreciation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDepreciation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDepreciation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDepreciation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of TempDepreciation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDepreciation <- function(TempDepreciationID, AssetID = F, Date = F, BaseCurrencyAmount = F, AssetTagNumber = F, AssetNumber = F, Item = F, AccountIDDepreciationExpense = F, AccountIDAccumulatedDepreciation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AssetDescription = F, IsError = F, ErrorMessage = F, AccountSummaryAccumulatedDepreciation = F, AccountSummaryDepreciationExpense = F, AccountSource = F, HasDefaultAccumulatedDepreciationAccount = F, HasDefaultDepreciationExpenseAccount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDepreciationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "TempDepreciation", objectId = TempDepreciationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDepreciation
	#'
	#' This function deletes a TempDepreciation
	#' @param TempDepreciationID The ID of the TempDepreciation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The TempDepreciationID of the deleted TempDepreciation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDepreciation <- function(TempDepreciationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "TempDepreciation", objectId = TempDepreciationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDepreciation
	#'
	#' This function creates a TempDepreciation
	#' @param fieldNames The field values to give the created TempDepreciation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created TempDepreciation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDepreciation <- function(AssetID = NULL, Date = NULL, BaseCurrencyAmount = NULL, AssetTagNumber = NULL, AssetNumber = NULL, Item = NULL, AccountIDDepreciationExpense = NULL, AccountIDAccumulatedDepreciation = NULL, AssetDescription = NULL, IsError = NULL, ErrorMessage = NULL, AccountSummaryAccumulatedDepreciation = NULL, AccountSummaryDepreciationExpense = NULL, AccountSource = NULL, HasDefaultAccumulatedDepreciationAccount = NULL, HasDefaultDepreciationExpenseAccount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "TempDepreciation", body = list(DataObject = body), searchFields = append("TempDepreciationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDepreciation
	#'
	#' This function modifies a TempDepreciation
	#' @param fieldNames The field values to give the modified TempDepreciation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified TempDepreciation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDepreciation <- function(TempDepreciationID, AssetID = NULL, Date = NULL, BaseCurrencyAmount = NULL, AssetTagNumber = NULL, AssetNumber = NULL, Item = NULL, AccountIDDepreciationExpense = NULL, AccountIDAccumulatedDepreciation = NULL, AssetDescription = NULL, IsError = NULL, ErrorMessage = NULL, AccountSummaryAccumulatedDepreciation = NULL, AccountSummaryDepreciationExpense = NULL, AccountSource = NULL, HasDefaultAccumulatedDepreciationAccount = NULL, HasDefaultDepreciationExpenseAccount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "TempDepreciation", objectId = TempDepreciationID, body = list(DataObject = body), searchFields = append("TempDepreciationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAdditionDisposals
	#'
	#' This function returns a dataframe or json object of TempAdditionDisposals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAdditionDisposals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAdditionDisposals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAdditionDisposal') to get more field paths.
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
	#' @concept Asset
	#' @return A list of TempAdditionDisposals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAdditionDisposals <- function(searchConditionsList = NULL, TempAdditionDisposalID = F, AssetID = F, Date = F, BaseCurrencyAmount = F, AccountIDAsset = F, AccountIDSource = F, IsError = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "TempAdditionDisposal", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAdditionDisposal
	#'
	#' This function returns a dataframe or json object of a TempAdditionDisposal
	#' @param TempAdditionDisposalID The ID of the TempAdditionDisposal to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAdditionDisposal. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAdditionDisposal.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAdditionDisposal') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of TempAdditionDisposal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAdditionDisposal <- function(TempAdditionDisposalID, AssetID = F, Date = F, BaseCurrencyAmount = F, AccountIDAsset = F, AccountIDSource = F, IsError = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAdditionDisposalID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "TempAdditionDisposal", objectId = TempAdditionDisposalID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAdditionDisposal
	#'
	#' This function deletes a TempAdditionDisposal
	#' @param TempAdditionDisposalID The ID of the TempAdditionDisposal to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The TempAdditionDisposalID of the deleted TempAdditionDisposal.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAdditionDisposal <- function(TempAdditionDisposalID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "TempAdditionDisposal", objectId = TempAdditionDisposalID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAdditionDisposal
	#'
	#' This function creates a TempAdditionDisposal
	#' @param fieldNames The field values to give the created TempAdditionDisposal. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created TempAdditionDisposal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAdditionDisposal <- function(AssetID = NULL, Date = NULL, BaseCurrencyAmount = NULL, AccountIDAsset = NULL, AccountIDSource = NULL, IsError = NULL, ErrorMessage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "TempAdditionDisposal", body = list(DataObject = body), searchFields = append("TempAdditionDisposalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAdditionDisposal
	#'
	#' This function modifies a TempAdditionDisposal
	#' @param fieldNames The field values to give the modified TempAdditionDisposal. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified TempAdditionDisposal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAdditionDisposal <- function(TempAdditionDisposalID, AssetID = NULL, Date = NULL, BaseCurrencyAmount = NULL, AccountIDAsset = NULL, AccountIDSource = NULL, IsError = NULL, ErrorMessage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "TempAdditionDisposal", objectId = TempAdditionDisposalID, body = list(DataObject = body), searchFields = append("TempAdditionDisposalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssetClasses
	#'
	#' This function returns a dataframe or json object of AssetClasses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssetClasses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssetClasses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssetClass') to get more field paths.
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
	#' @concept Asset
	#' @return A list of AssetClasses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssetClasses <- function(searchConditionsList = NULL, AssetClassID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "AssetClass", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssetClass
	#'
	#' This function returns a dataframe or json object of an AssetClass
	#' @param AssetClassID The ID of the AssetClass to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssetClass. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssetClass.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssetClass') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of AssetClass
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssetClass <- function(AssetClassID, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssetClassID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "AssetClass", objectId = AssetClassID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssetClass
	#'
	#' This function deletes an AssetClass
	#' @param AssetClassID The ID of the AssetClass to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The AssetClassID of the deleted AssetClass.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssetClass <- function(AssetClassID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "AssetClass", objectId = AssetClassID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssetClass
	#'
	#' This function creates an AssetClass
	#' @param fieldNames The field values to give the created AssetClass. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created AssetClass
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssetClass <- function(DistrictID = NULL, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "AssetClass", body = list(DataObject = body), searchFields = append("AssetClassID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssetClass
	#'
	#' This function modifies an AssetClass
	#' @param fieldNames The field values to give the modified AssetClass. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified AssetClass
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssetClass <- function(AssetClassID, DistrictID = NULL, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "AssetClass", objectId = AssetClassID, body = list(DataObject = body), searchFields = append("AssetClassID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssetClassAccounts
	#'
	#' This function returns a dataframe or json object of AssetClassAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssetClassAccounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssetClassAccounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssetClassAccount') to get more field paths.
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
	#' @concept Asset
	#' @return A list of AssetClassAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssetClassAccounts <- function(searchConditionsList = NULL, AssetClassAccountID = F, AssetClassID = F, AccountID = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "AssetClassAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssetClassAccount
	#'
	#' This function returns a dataframe or json object of an AssetClassAccount
	#' @param AssetClassAccountID The ID of the AssetClassAccount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssetClassAccount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssetClassAccount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssetClassAccount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of AssetClassAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssetClassAccount <- function(AssetClassAccountID, AssetClassID = F, AccountID = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssetClassAccountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "AssetClassAccount", objectId = AssetClassAccountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssetClassAccount
	#'
	#' This function deletes an AssetClassAccount
	#' @param AssetClassAccountID The ID of the AssetClassAccount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The AssetClassAccountID of the deleted AssetClassAccount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssetClassAccount <- function(AssetClassAccountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "AssetClassAccount", objectId = AssetClassAccountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssetClassAccount
	#'
	#' This function creates an AssetClassAccount
	#' @param fieldNames The field values to give the created AssetClassAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created AssetClassAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssetClassAccount <- function(AssetClassID = NULL, AccountID = NULL, Type = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "AssetClassAccount", body = list(DataObject = body), searchFields = append("AssetClassAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssetClassAccount
	#'
	#' This function modifies an AssetClassAccount
	#' @param fieldNames The field values to give the modified AssetClassAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified AssetClassAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssetClassAccount <- function(AssetClassAccountID, AssetClassID = NULL, AccountID = NULL, Type = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "AssetClassAccount", objectId = AssetClassAccountID, body = list(DataObject = body), searchFields = append("AssetClassAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List VendorItems
	#'
	#' This function returns a dataframe or json object of VendorItems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given VendorItems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the VendorItems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('VendorItem') to get more field paths.
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
	#' @concept Asset
	#' @return A list of VendorItems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listVendorItems <- function(searchConditionsList = NULL, VendorItemID = F, ItemID = F, VendorID = F, IsPrimaryVendor = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "VendorItem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a VendorItem
	#'
	#' This function returns a dataframe or json object of a VendorItem
	#' @param VendorItemID The ID of the VendorItem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given VendorItem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the VendorItem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('VendorItem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of VendorItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getVendorItem <- function(VendorItemID, ItemID = F, VendorID = F, IsPrimaryVendor = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "VendorItemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "VendorItem", objectId = VendorItemID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a VendorItem
	#'
	#' This function deletes a VendorItem
	#' @param VendorItemID The ID of the VendorItem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The VendorItemID of the deleted VendorItem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteVendorItem <- function(VendorItemID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "VendorItem", objectId = VendorItemID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a VendorItem
	#'
	#' This function creates a VendorItem
	#' @param fieldNames The field values to give the created VendorItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created VendorItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createVendorItem <- function(ItemID = NULL, VendorID = NULL, IsPrimaryVendor = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "VendorItem", body = list(DataObject = body), searchFields = append("VendorItemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a VendorItem
	#'
	#' This function modifies a VendorItem
	#' @param fieldNames The field values to give the modified VendorItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified VendorItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyVendorItem <- function(VendorItemID, ItemID = NULL, VendorID = NULL, IsPrimaryVendor = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "VendorItem", objectId = VendorItemID, body = list(DataObject = body), searchFields = append("VendorItemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDepreciationAccountings
	#'
	#' This function returns a dataframe or json object of TempDepreciationAccountings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDepreciationAccountings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDepreciationAccountings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDepreciationAccounting') to get more field paths.
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
	#' @concept Asset
	#' @return A list of TempDepreciationAccountings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDepreciationAccountings <- function(searchConditionsList = NULL, TempDepreciationAccountingID = F, TempDepreciationID = F, AccountIDAccumulatedDepreciation = F, AccountIDDepreciationExpense = F, Percentage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Asset", objectName = "TempDepreciationAccounting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDepreciationAccounting
	#'
	#' This function returns a dataframe or json object of a TempDepreciationAccounting
	#' @param TempDepreciationAccountingID The ID of the TempDepreciationAccounting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDepreciationAccounting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDepreciationAccounting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDepreciationAccounting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A dataframe or of TempDepreciationAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDepreciationAccounting <- function(TempDepreciationAccountingID, TempDepreciationID = F, AccountIDAccumulatedDepreciation = F, AccountIDDepreciationExpense = F, Percentage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDepreciationAccountingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Asset", objectName = "TempDepreciationAccounting", objectId = TempDepreciationAccountingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDepreciationAccounting
	#'
	#' This function deletes a TempDepreciationAccounting
	#' @param TempDepreciationAccountingID The ID of the TempDepreciationAccounting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The TempDepreciationAccountingID of the deleted TempDepreciationAccounting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDepreciationAccounting <- function(TempDepreciationAccountingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Asset", objectName = "TempDepreciationAccounting", objectId = TempDepreciationAccountingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDepreciationAccounting
	#'
	#' This function creates a TempDepreciationAccounting
	#' @param fieldNames The field values to give the created TempDepreciationAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return A newly created TempDepreciationAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDepreciationAccounting <- function(TempDepreciationID = NULL, AccountIDAccumulatedDepreciation = NULL, AccountIDDepreciationExpense = NULL, Percentage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Asset", objectName = "TempDepreciationAccounting", body = list(DataObject = body), searchFields = append("TempDepreciationAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDepreciationAccounting
	#'
	#' This function modifies a TempDepreciationAccounting
	#' @param fieldNames The field values to give the modified TempDepreciationAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Asset
	#' @return The modified TempDepreciationAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDepreciationAccounting <- function(TempDepreciationAccountingID, TempDepreciationID = NULL, AccountIDAccumulatedDepreciation = NULL, AccountIDDepreciationExpense = NULL, Percentage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Asset", objectName = "TempDepreciationAccounting", objectId = TempDepreciationAccountingID, body = list(DataObject = body), searchFields = append("TempDepreciationAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
