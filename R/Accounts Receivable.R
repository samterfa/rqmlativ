

	listAccountsReceivableAccruals <- function(searchConditionsList = NULL, AccountsReceivableAccrualID = F, Code = F, Description = F, CodeDescription = F, MaskID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "AccountsReceivableAccrual", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAgingThresholds <- function(searchConditionsList = NULL, AgingThresholdID = F, Description = F, StartNumberOfDays = F, EndNumberOfDays = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DistrictID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "AgingThreshold", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsReceivableConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, InvoiceGroupLength = F, InvoiceNumberLength = F, BatchDefault = F, AccountIDUnapplied = F, NameIDBusinessOffice = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportIDInvoiceDelivery = F, ReportIDStatementDelivery = F, EmailSendingAddressInvoice = F, EmailSendingAliasInvoice = F, EmailSendingAddressStatement = F, EmailSendingAliasStatement = F, InvoiceEmailSubject = F, InvoiceEmailBody = F, StatementEmailSubject = F, StatementEmailBody = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "ConfigDistrict", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPayorDistricts <- function(searchConditionsList = NULL, PayorDistrictID = F, PayorID = F, DistrictID = F, AccumulatedBalance = F, AmountDue = F, AmountPaid = F, UnappliedAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "PayorDistrict", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsReceivablePayors <- function(searchConditionsList = NULL, PayorID = F, NameID = F, AccountsReceivableContact = F, QuickKey = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FullNameLFM = F, FullNameFL = F, FullNameFML = F, DefaultDeliveryType = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "Payor", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDiscountDetails <- function(searchConditionsList = NULL, DiscountDetailID = F, DiscountID = F, DaysBeforeDueDate = F, Percent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "DiscountDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsReceivableDiscounts <- function(searchConditionsList = NULL, DiscountID = F, DistrictID = F, Code = F, Description = F, MaskIDDiscount = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "Discount", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsReceivableInvoices <- function(searchConditionsList = NULL, InvoiceID = F, FiscalYearID = F, PayorID = F, InvoiceGroupID = F, BankAccountID = F, AccountingUpdateIDBatch = F, Description = F, InvoiceNumber = F, Batch = F, InvoiceDate = F, DueDate = F, Status = F, Amount = F, CanClone = F, CanChangeAccount = F, CanMoveToOpen = F, CanMoveToHistory = F, BaseCurrencyAmountByAccount = F, AmountDue = F, AmountPaid = F, FormattedDescription = F, AgingDays = F, AttachmentCount = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CanDelete = F, InvoiceThirdPartyImportID = F, DeliveryStatus = F, DeliveryCount = F, LastDeliveryTime = F, IsPrinted = F, IsEmailed = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "Invoice", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsReceivableInvoiceAccountings <- function(searchConditionsList = NULL, InvoiceAccountingID = F, InvoiceDetailID = F, AccountID = F, BaseCurrencyAmount = F, PreviousGrossPaymentAmount = F, PreviousPaymentAmount = F, PreviousDiscountAmount = F, AmountDue = F, FormattedDescription = F, BaseCurrencyPercent = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GrantID = F, ProjectID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceAccounting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsReceivableInvoiceDetails <- function(searchConditionsList = NULL, InvoiceDetailID = F, InvoiceID = F, ItemID = F, UnitOfMeasureID = F, DiscountID = F, InvoiceDetailIDDeletedHistory = F, Description = F, Quantity = F, UnitCost = F, BaseCurrencyAmount = F, DisplayOrder = F, InvoiceTotal = F, DetailType = F, AmountDue = F, CanDelete = F, ItemCodeDetailDescription = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PurchaseOrderNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsReceivableInvoiceGroups <- function(searchConditionsList = NULL, InvoiceGroupID = F, GroupIDAccount = F, DistrictID = F, Code = F, Description = F, IsActive = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsReceivableInvoiceGroupClearances <- function(searchConditionsList = NULL, InvoiceGroupClearanceID = F, InvoiceGroupID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceGroupClearance", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsReceivableItems <- function(searchConditionsList = NULL, ItemID = F, DistrictID = F, DiscountID = F, Code = F, Description = F, AccountID = F, UnitOfMeasureID = F, AccountsReceivableAccrualID = F, CodeDescription = F, UnitCost = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "Item", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listNextInvoiceNumbers <- function(searchConditionsList = NULL, NextInvoiceNumberID = F, InvoiceGroupID = F, FiscalYearID = F, InvoiceNumber = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "NextInvoiceNumber", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsReceivableTempInvoices <- function(searchConditionsList = NULL, TempInvoiceID = F, ErrorCount = F, InvoiceID = F, InvoiceGroupID = F, InvoiceGroupCodeDescription = F, FiscalYearID = F, FiscalYearDescription = F, PayorID = F, PayorLFM = F, BankAccountID = F, BankAccountCodeDescription = F, Description = F, Amount = F, Batch = F, InvoiceNumber = F, InvoiceDate = F, DueDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasErrors = F, LineNumber = F, DeliveryType = F, DeliveryStatus = F, AmountPaid = F, AmountDue = F, AccountingUpdateIDBatch = F, BatchPostDate = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "TempInvoice", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempInvoiceErrorDetails <- function(searchConditionsList = NULL, TempInvoiceErrorDetailID = F, TempInvoiceID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LineNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "TempInvoiceErrorDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceThirdPartyFormats <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatID = F, SkywardID = F, SkywardHash = F, SkywardIDClonedFrom = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, IsSystemLoaded = F, ImportType = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DefaultBankAccountID = F, DefaultInvoiceGroupID = F, PayorIdentification = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormat", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceThirdPartyImports <- function(searchConditionsList = NULL, InvoiceThirdPartyImportID = F, ImportTime = F, ImportData = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InvoiceThirdPartyFormatID = F, MediaIDFailedResult = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyImport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceThirdPartyFormatInvoiceGroups <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatInvoiceGroupID = F, InvoiceGroupID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatInvoiceGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceThirdPartyFormatPayors <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatPayorID = F, PayorID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatPayor", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceThirdPartyFormatItems <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatItemID = F, ItemID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatItem", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceThirdPartyFormatDiscounts <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatDiscountID = F, DiscountID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatDiscount", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceThirdPartyFormatBankAccounts <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatBankAccountID = F, BankAccountID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatBankAccount", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceThirdPartyFormatAccounts <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatAccountID = F, AccountID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatAccount", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceThirdPartyFormatUnitOfMeasures <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatUnitOfMeasureID = F, UnitOfMeasureID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatUnitOfMeasure", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceDelimitedFileFormats <- function(searchConditionsList = NULL, InvoiceDelimitedFileFormatID = F, SkywardID = F, SkywardHash = F, InvoiceThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterType = F, DelimiterCharacter = F, InvoiceGroupColumnNumber = F, PayorColumnNumber = F, BankAccountColumnNumber = F, BatchColumnNumber = F, InvoiceDateColumnNumber = F, DueDateColumnNumber = F, ItemColumnNumber = F, QuantityColumnNumber = F, UnitOfMeasureColumnNumber = F, UnitCostColumnNumber = F, DiscountColumnNumber = F, AccountColumnNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InvoiceDescriptionColumnNumber = F, InvoiceDetailDescriptionColumnNumber = F, PurchaseOrderNumberColumnNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceDelimitedFileFormat", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceFixedLengthFileFormats <- function(searchConditionsList = NULL, InvoiceFixedLengthFileFormatID = F, SkywardID = F, SkywardHash = F, InvoiceThirdPartyFormatID = F, NumberOfHeaderRows = F, InvoiceGroupStartPosition = F, InvoiceGroupLength = F, PayorStartPosition = F, PayorLength = F, BankAccountStartPosition = F, BankAccountLength = F, InvoiceDescriptionStartPosition = F, InvoiceDescriptionLength = F, BatchStartPosition = F, BatchLength = F, InvoiceDateStartPosition = F, InvoiceDateLength = F, DueDateStartPosition = F, DueDateLength = F, ItemStartPosition = F, ItemLength = F, InvoiceDetailDescriptionStartPosition = F, InvoiceDetailDescriptionLength = F, QuantityStartPosition = F, QuantityLength = F, UnitOfMeasureStartPosition = F, UnitOfMeasureLength = F, UnitCostStartPosition = F, UnitCostLength = F, DiscountStartPosition = F, DiscountLength = F, AccountStartPosition = F, AccountLength = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PurchaseOrderNumberStartPosition = F, PurchaseOrderNumberLength = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceFixedLengthFileFormat", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsReceivableTempExceptions <- function(searchConditionsList = NULL, TempExceptionID = F, Message = F, LineNumber = F, IsFatal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ParentObjectID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "TempException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsReceivableTempInvoiceDetails <- function(searchConditionsList = NULL, TempInvoiceDetailID = F, TempInvoiceID = F, ItemID = F, ItemCodeDescription = F, AccountID = F, AccountDistribution = F, Description = F, Quantity = F, UnitCost = F, BaseCurrencyAmount = F, DiscountID = F, DiscountCodeDescription = F, UnitOfMeasureID = F, UnitOfMeasureCodeDescription = F, LineNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PurchaseOrderNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "TempInvoiceDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listConfigDistrictEmailTypes <- function(searchConditionsList = NULL, ConfigDistrictEmailTypeID = F, EmailTypeID = F, ConfigDistrictID = F, Rank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "ConfigDistrictEmailType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDeliveryExceptions <- function(searchConditionsList = NULL, DeliveryExceptionID = F, InvoiceDeliveryID = F, StatementDeliveryID = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "DeliveryException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceDeliveries <- function(searchConditionsList = NULL, InvoiceDeliveryID = F, InvoiceID = F, UserIDSentBy = F, MediaID = F, SentTime = F, DeliveryType = F, Status = F, EmailRecipient = F, EmailSendingAddress = F, EmailSendingAlias = F, EmailBody = F, EmailSubject = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceDelivery", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStatementDeliveries <- function(searchConditionsList = NULL, StatementDeliveryID = F, PayorID = F, UserIDSentBy = F, MediaID = F, SentTime = F, DeliveryType = F, Status = F, EmailRecipient = F, EmailSendingAddress = F, EmailSendingAlias = F, EmailBody = F, EmailSubject = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "StatementDelivery", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPayors <- function(searchConditionsList = NULL, TempPayorID = F, PayorID = F, LastName = F, FirstName = F, MiddleName = F, QuickKey = F, AmountDue = F, UnappliedAmount = F, AccountsReceivableContact = F, MailingAddress = F, DeliveryType = F, HasErrors = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "TempPayor", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}
