

	listAccountsPayableRuns <- function(searchConditionsList = NULL, AccountsPayableRunID = F, DistrictID = F, BudgetaryPostDate = F, CheckDate = F, BankAccountID = F, Description = F, Status = F, PrintStatus = F, Type = F, AccountingUpdateID = F, WillCreateACH = F, TotalPayoutAmount = F, CheckTransactionCount = F, AttachmentCount = F, RenderHistoryUnprintedOption = F, RenderDelete = F, RenderSelectInvoices = F, RenderSelectInvoicesVoid = F, RenderCalculate = F, RenderAccountingRegister = F, RenderUpdate = F, RenderUpdateVoid = F, RenderAccountingRegisterVoid = F, RenderPurge = F, RenderPurgeVoid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PaymentDateFilter = F, StartingACHNumber = F, EndingACHNumber = F, StartingCheckNumber = F, EndingCheckNumber = F, CheckDateYear = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "AccountsPayableRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsPayableRunExceptions <- function(searchConditionsList = NULL, AccountsPayableRunExceptionID = F, AccountsPayableRunID = F, VendorID = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "AccountsPayableRunException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsPayableCheckTransactions <- function(searchConditionsList = NULL, CheckTransactionID = F, AccountsPayableRunID = F, VendorID = F, NetCheckAmount = F, CheckTransactionIDVoidedFrom = F, CheckNumberOverride = F, CheckNumber = F, PaymentType = F, IsVoided = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsACHOnBankReconciliation = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CheckTransaction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsPayableConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, SendInvoiceApprovedMessage = F, InvoiceRequestApprovedMessageSubject = F, InvoiceRequestApprovedMessageContent = F, SendInvoiceDeniedMessage = F, InvoiceRequestDeniedMessageSubject = F, InvoiceRequestDeniedMessageContent = F, SendInvoiceWaitingMessage = F, InvoiceRequestWaitingMessageSubject = F, InvoiceRequestWaitingMessageContent = F, SendCreditCardTransactionPreApprovalApprovedMessage = F, SendCreditCardTransactionPostApprovalApprovedMessage = F, CreditCardTransactionRequestApprovedMessageSubject = F, CreditCardTransactionRequestApprovedMessageContent = F, SendCreditCardTransactionPreApprovalDeniedMessage = F, SendCreditCardTransactionPostApprovalDeniedMessage = F, CreditCardTransactionRequestDeniedMessageSubject = F, CreditCardTransactionRequestDeniedMessageContent = F, SendCreditCardTransactionPreApprovalWaitingMessage = F, SendCreditCardTransactionPostApprovalWaitingMessage = F, CreditCardTransactionRequestWaitingMessageSubject = F, CreditCardTransactionRequestWaitingMessageContent = F, MaskIDAccrual = F, UseAvailableFundWarning = F, UseAvailableFundError = F, PercentOverPurchaseOrder = F, AvailableFundWarningXMLFilter = F, SecurityStandardWarningFilterData = F, AvailableFundErrorXMLFilter = F, SecurityStandardErrorFilterData = F, BatchDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SendExpenseReimbursementPreApprovalApprovedMessage = F, SendExpenseReimbursementPostApprovalApprovedMessage = F, ExpenseReimbursementRequestApprovedMessageSubject = F, ExpenseReimbursementRequestApprovedMessageContent = F, SendExpenseReimbursementPreApprovalDeniedMessage = F, SendExpenseReimbursementPostApprovalDeniedMessage = F, ExpenseReimbursementRequestDeniedMessageSubject = F, ExpenseReimbursementRequestDeniedMessageContent = F, SendExpenseReimbursementPreApprovalWaitingMessage = F, SendExpenseReimbursementPostApprovalWaitingMessage = F, ExpenseReimbursementRequestWaitingMessageSubject = F, ExpenseReimbursementRequestWaitingMessageContent = F, OneFundPerCheck = F, DirectDepositNotificationSubject = F, DirectDepositNotificationContent = F, MaskIDAccrualActivityAccounting = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ConfigDistrict", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCardCheckouts <- function(searchConditionsList = NULL, CreditCardCheckoutID = F, CreditCardID = F, EffectiveTime = F, UserIDCheckoutBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasCreditCardGroupAccess = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardCheckout", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listExpenseReimbursements <- function(searchConditionsList = NULL, ExpenseReimbursementID = F, FiscalYearID = F, Status = F, TransactionStartDate = F, TransactionEndDate = F, NumberOfTransactionDays = F, EntryAmount = F, BaseCurrencyAmount = F, CurrencyIDEntry = F, Description = F, AttachmentCount = F, NameIDReimbursementFor = F, ExpenseReimbursementGroupID = F, CanSubmit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InvoiceID = F, AssignmentIDReimbursementFor = F, IsDataComplete = F, BaseCurrencyAmountByAccount = F, CurrentUserHasExpenseReimbursementGroupAccess = F, RenderDeleteButton = F, RenderResubmitButton = F, RenderExpenseReimbursementDetailBrowseButtons = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursement", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listExpenseReimbursementDetails <- function(searchConditionsList = NULL, ExpenseReimbursementDetailID = F, ExpenseReimbursementTypeID = F, ExpenseReimbursementID = F, Reimburse = F, DisplayOrder = F, Description = F, Quantity = F, UnitCost = F, EntryAmount = F, BaseCurrencyAmount = F, ExpenseReimbursementTotalLessEntryAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Date = F, QuantityAndLabel = F, RateAndLabel = F, CurrentUserHasExpenseReimbursementGroupAccess = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listExpenseReimbursementAccountings <- function(searchConditionsList = NULL, ExpenseReimbursementAccountingID = F, ExpenseReimbursementDetailID = F, EntryAmount = F, BaseCurrencyAmount = F, BaseCurrencyPercent = F, AccountID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasAccountGroupAccess = F, CurrentUserHasExpenseReimbursementGroupAccess = F, GrantID = F, ProjectID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementAccounting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listExpenseReimbursementGroups <- function(searchConditionsList = NULL, ExpenseReimbursementGroupID = F, DistrictID = F, GroupIDAccount = F, Code = F, Description = F, CodeDescription = F, IsActive = F, IsPreApprovalWorkflowUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsPostApprovalWorkflowUpdated = F, CurrentUserHasExpenseReimbursementGroupAccess = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listExpenseReimbursementGroupClearances <- function(searchConditionsList = NULL, ExpenseReimbursementGroupClearanceID = F, ExpenseReimbursementGroupID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupClearance", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listExpenseReimbursementTypes <- function(searchConditionsList = NULL, ExpenseReimbursementTypeID = F, ExpenseReimbursementGroupID = F, Code = F, Description = F, CodeDescription = F, TransactionStartDate = F, TransactionEndDate = F, QuantityLabel = F, QuantityPrecision = F, QuantityDefaultValue = F, IsQuantityReadOnly = F, RateLabel = F, RatePrecision = F, RateDefaultValue = F, MaximumRate = F, IsRateReadOnly = F, AllowEmployeeAccess = F, XMLFilter = F, SecurityStandardFilterData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FormattedQuantityDefaultValue = F, FormattedRateDefaultValue = F, FormattedMaximumRate = F, CurrentUserHasExpenseReimbursementGroupAccess = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceApprovals <- function(searchConditionsList = NULL, InvoiceApprovalID = F, InvoiceID = F, UserIDApprover = F, Comment = F, TaskInstanceID = F, Status = F, Level = F, LevelDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ApprovalActionTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceApproval", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsPayableInvoiceGroups <- function(searchConditionsList = NULL, InvoiceGroupID = F, DistrictID = F, GroupIDAccount = F, Code = F, Description = F, IsActive = F, CodeDescription = F, IsApprovalWorkflowUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasInvoiceGroupAccess = F, ActivityAccountingGroupID = F, IsActivityAccounting = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceGroupApprovalTasks <- function(searchConditionsList = NULL, InvoiceGroupApprovalTaskID = F, InvoiceGroupID = F, IsConditional = F, Level = F, Description = F, XMLFilter = F, SecurityStandardFilterData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceGroupApprovalTask", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceGroupApprovalTaskSecurityGroups <- function(searchConditionsList = NULL, InvoiceGroupApprovalTaskSecurityGroupID = F, InvoiceGroupApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceGroupApprovalTaskSecurityGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listInvoiceGroupBankAccounts <- function(searchConditionsList = NULL, InvoiceGroupBankAccountID = F, InvoiceGroupID = F, BankAccountID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceGroupBankAccount", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsPayableInvoiceGroupClearances <- function(searchConditionsList = NULL, InvoiceGroupClearanceID = F, InvoiceGroupID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceGroupClearance", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCardTransactionApprovals <- function(searchConditionsList = NULL, CreditCardTransactionApprovalID = F, CreditCardTransactionID = F, UserIDApprover = F, Comment = F, TaskInstanceID = F, Status = F, Level = F, Type = F, LevelDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ApprovalActionTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionApproval", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCardGroupApprovalTasks <- function(searchConditionsList = NULL, CreditCardGroupApprovalTaskID = F, CreditCardGroupID = F, IsConditional = F, Level = F, Description = F, IncludeCreditCardCheckoutUser = F, XMLFilter = F, SecurityStandardFilterData = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardGroupApprovalTask", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCardGroupApprovalTaskSecurityGroups <- function(searchConditionsList = NULL, CreditCardGroupApprovalTaskSecurityGroupID = F, CreditCardGroupApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardGroupApprovalTaskSecurityGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempInvoiceSubmitErrors <- function(searchConditionsList = NULL, TempInvoiceSubmitErrorID = F, VendorID = F, VendorName = F, Description = F, InvoiceNumber = F, Batch = F, Amount = F, FiscalYear = F, PurchaseOrderNumber = F, InvoiceGroup = F, ErrorMessage = F, Severity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceSubmitError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempInvoiceBatches <- function(searchConditionsList = NULL, TempInvoiceBatchID = F, BankAccountID = F, Batch = F, RegularAmount = F, WireTransferAmount = F, ACHAmount = F, VoidAmount = F, InvoiceCount = F, FiscalYearDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ManualCheckAmount = F, EpayableCheckAmount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceBatch", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempInvoiceJuryDutyErrors <- function(searchConditionsList = NULL, TempInvoiceJuryDutyErrorID = F, FullName = F, ObjectName = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceJuryDutyError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempInvoiceJuryDuties <- function(searchConditionsList = NULL, TempInvoiceJuryDutyID = F, ExistingVendorText = F, ReadOnlyDemographicInfo = F, ReadOnlyVendorFields = F, VendorID = F, VendorNumber = F, NameID = F, FirstName = F, MiddleName = F, LastName = F, AddressLine1 = F, AddressLine2 = F, City = F, StateCode = F, ZipCode = F, Amount = F, SourceData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceJuryDuty", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsPayableInvoices <- function(searchConditionsList = NULL, InvoiceID = F, Batch = F, InvoiceDate = F, Description = F, InvoiceNumber = F, AccountingUpdateIDExpense = F, AccountingUpdateIDCash = F, ContractID = F, CommodityID = F, PurchaseOrderID = F, DistrictID = F, VendorID = F, FiscalYearID = F, CheckTransactionID = F, BankAccountID = F, CurrencyIDEntry = F, AccountsPayableRunID = F, PayrollRunID = F, CurrencyConversionRate = F, InvoiceGroupID = F, PendingReceiving = F, Status = F, PaymentType = F, SourceType = F, AccountsPayableRunType = F, EntryAmount = F, BaseCurrencyAmount = F, BaseCurrencyAmountByAccount = F, CanChangeAccount = F, CanClone = F, CanSubmit = F, ShippingAmount = F, EntryTotal = F, RenderInvoiceUpdateVoid = F, AttachmentCount = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DueDate = F, CanDelete = F, CanDeleteInvoiceDetail = F, CanReSubmit = F, PaymentTermsID = F, DiscountPercentApplied = F, PaymentTermsDate = F, DiscountAmount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "Invoice", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsPayableInvoiceAccountings <- function(searchConditionsList = NULL, InvoiceAccountingID = F, InvoiceDetailID = F, AccountID = F, EntryAmount = F, BaseCurrencyAmount = F, Percent = F, BaseCurrencyPercent = F, BaseCurrencyAmountByAccount = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasAccountGroupAccess = F, GrantID = F, ProjectID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceAccounting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsPayableInvoiceDetails <- function(searchConditionsList = NULL, InvoiceDetailID = F, InvoiceID = F, DisplayOrder = F, Quantity = F, UnitCost = F, EntryAmount = F, BaseCurrencyAmount = F, CommodityID = F, CatalogItem = F, Description = F, PurchaseOrderDetailID = F, UnitOfMeasureID = F, InvoiceDetailIDDeletedHistory = F, EntryShippingAmount = F, IsOnPurchaseOrder = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAsset = F, InvoiceTotalLessEntryAmount = F, EntryDiscountAmount = F, DiscountAmount = F, Form1099TypeID = F, Form1099TypeBoxNumberDescription = F, Form1099TypeBoxNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCheckTransactions <- function(searchConditionsList = NULL, TempCheckTransactionID = F, VendorName = F, PaymentType = F, NetCheckAmount = F, AccountsPayableContact = F, RemitToAddress = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayAccountsPayableContact = F, PrintAPContact = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempCheckTransaction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempInvoiceAccountingErrors <- function(searchConditionsList = NULL, TempInvoiceAccountingErrorID = F, Type = F, Code = F, VendorFullNameLFM = F, DisplayAccount = F, Error = F, InvoiceDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceAccountingError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsPayableTempInvoices <- function(searchConditionsList = NULL, TempInvoiceID = F, VendorID = F, VendorName = F, Description = F, InvoiceNumber = F, Batch = F, TotalAmount = F, DeductionID = F, BenefitID = F, PayrollRunID = F, PaymentTypeCode = F, SourceType = F, SourceCode = F, SourceDisplayAccount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasError = F, InvoiceGroupID = F, BankAccountID = F, FundDimensionID = F, InvoiceGroupDescription = F, InvoiceDataObjectVariableIdentifier = F, Fund = F, FiscalYear = F, BillID = F, InvoiceDate = F, DueDate = F, AccountID = F, BaseCurrencyAmount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoice", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCards <- function(searchConditionsList = NULL, CreditCardID = F, DistrictID = F, Description = F, Number = F, NumberForDisplay = F, SelectedNumberForDisplay = F, Alias = F, ExpirationDate = F, CreditCardGroupID = F, VendorID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MaximumAmount = F, IsActive = F, CurrentUserHasCreditCardGroupAccess = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCard", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCardGroups <- function(searchConditionsList = NULL, CreditCardGroupID = F, DistrictID = F, GroupIDAccount = F, Code = F, Description = F, CodeDescription = F, IsActive = F, IsPreApprovalWorkflowUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsPostApprovalWorkflowUpdated = F, CurrentUserHasCreditCardGroupAccess = F, ActivityAccountingGroupID = F, IsActivityAccounting = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCardGroupClearances <- function(searchConditionsList = NULL, CreditCardGroupClearanceID = F, CreditCardGroupID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardGroupClearance", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCardTransactions <- function(searchConditionsList = NULL, CreditCardTransactionID = F, CreditCardID = F, FiscalYearID = F, InvoiceDetailID = F, VendorID = F, VendorDescription = F, Status = F, Type = F, TransactionTime = F, BaseCurrencyAmount = F, EntryAmount = F, CurrencyIDEntry = F, Description = F, CreditCardTransactionIDOriginal = F, UserIDCreditCardUsedBy = F, BaseCurrencyAmountByAccount = F, AttachmentCount = F, IsPreApproved = F, CanSubmit = F, IsDataComplete = F, RenderCreditCardTransactionDetailBrowseButtons = F, CanClone = F, CanChangeAccount = F, VendorDisplayName = F, SelectedValueDisplay = F, CurrentUserHasCreditCardGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CreditCardTransactionThirdPartyImportID = F, CanDelete = F, CanResubmit = F, CanSkipImport = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransaction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCardTransactionDetails <- function(searchConditionsList = NULL, CreditCardTransactionDetailID = F, CreditCardTransactionID = F, Description = F, EntryAmount = F, BaseCurrencyAmount = F, Quantity = F, UnitCost = F, DisplayOrder = F, CreditCardTransactionTotalLessEntryAmount = F, CurrentUserHasCreditCardGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCardTransactionDetailAccountings <- function(searchConditionsList = NULL, CreditCardTransactionDetailAccountingID = F, CreditCardTransactionDetailID = F, EntryAmount = F, BaseCurrencyAmount = F, AccountID = F, BaseCurrencyPercent = F, CurrentUserHasCreditCardGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasAccountGroupAccess = F, GrantID = F, ProjectID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionDetailAccounting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listExpenseReimbursementApprovals <- function(searchConditionsList = NULL, ExpenseReimbursementApprovalID = F, ExpenseReimbursementID = F, UserIDApprover = F, Comment = F, TaskInstanceID = F, Status = F, Level = F, Type = F, LevelDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OrganizationChartRelationshipID = F, ApprovalActionTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementApproval", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listExpenseReimbursementGroupApprovalTasks <- function(searchConditionsList = NULL, ExpenseReimbursementGroupApprovalTaskID = F, ExpenseReimbursementGroupID = F, IsConditional = F, Level = F, Description = F, XMLFilter = F, SecurityStandardFilterData = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UseOrganizationChart = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupApprovalTask", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listExpenseReimbursementGroupApprovalTaskSecurityGroups <- function(searchConditionsList = NULL, ExpenseReimbursementGroupApprovalTaskSecurityGroupID = F, ExpenseReimbursementGroupApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupApprovalTaskSecurityGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsPayableConfigFiscalYears <- function(searchConditionsList = NULL, ConfigFiscalYearID = F, FiscalYearID = F, DistrictID = F, OrganizationChartIDExpenseReimbursementApproval = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ConfigFiscalYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCreditCardTransactions <- function(searchConditionsList = NULL, TempCreditCardTransactionID = F, TransactionTime = F, CreditCardGroup = F, CreditCard = F, VendorID = F, VendorName = F, Status = F, Description = F, Amount = F, FiscalYear = F, ErrorMessage = F, Severity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CreditCardTransactionID = F, ErrorCount = F, VendorDescription = F, UserIDCreditCardUsedBy = F, UserCreditCardUsedBy = F, CreditCardID = F, FiscalYearID = F, HasErrors = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempCreditCardTransaction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCreditCardTransactionExceptionMessages <- function(searchConditionsList = NULL, TempCreditCardTransactionExceptionMessageID = F, TempCreditCardTransactionID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempCreditCardTransactionExceptionMessage", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempInvoiceErrors <- function(searchConditionsList = NULL, TempInvoiceErrorID = F, TempInvoiceID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCreditCardTransactionDetailAccountings <- function(searchConditionsList = NULL, TempCreditCardTransactionDetailAccountingID = F, Error = F, DisplayAccount = F, CreditCardTransaction = F, CreditCardTransactionDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Severity = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempCreditCardTransactionDetailAccounting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempExpenseReimbursementAccountings <- function(searchConditionsList = NULL, TempExpenseReimbursementAccountingID = F, Error = F, DisplayAccount = F, ExpenseReimbursement = F, ExpenseReimbursementDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempExpenseReimbursementAccounting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsPayableTempInvoiceDetails <- function(searchConditionsList = NULL, TempInvoiceDetailID = F, TempInvoiceID = F, Quantity = F, UnitCost = F, BaseCurrencyAmount = F, EntryAmount = F, Description = F, DisplayOrder = F, FirstAccountDistributionAccountID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCardTransactionDelimitedFileFormats <- function(searchConditionsList = NULL, CreditCardTransactionDelimitedFileFormatID = F, SkywardID = F, SkywardHash = F, CreditCardTransactionThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterType = F, DelimiterCharacter = F, TransactionImportIDColumnNumber = F, SequenceNumberColumnNumber = F, UniqueCardIdentifierColumnNumber = F, TransactionDateColumnNumber = F, TransactionAmountColumnNumber = F, SplitAmountColumnNumber = F, GeneralDefaultCodeOneColumnNumber = F, GeneralDefaultCodeTwoColumnNumber = F, GeneralDefaultCodeThreeColumnNumber = F, GeneralDefaultCodeFourColumnNumber = F, GeneralDefaultCodeFiveColumnNumber = F, GeneralDefaultCodeSixColumnNumber = F, GeneralDefaultCodeSevenColumnNumber = F, GeneralDefaultCodeEightColumnNumber = F, GeneralDefaultCodeNineColumnNumber = F, GeneralDefaultCodeTenColumnNumber = F, GeneralDefaultCodeElevenColumnNumber = F, GeneralDefaultCodeTwelveColumnNumber = F, VendorNameColumnNumber = F, VendorCityColumnNumber = F, VendorStateProvinceColumnNumber = F, VendorZipCodeColumnNumber = F, VendorCountryColumnNumber = F, TransactionDescriptionColumnNumber = F, SplitDescriptionColumnNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UniqueAccountIdentifierColumnNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionDelimitedFileFormat", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCardTransactionFixedLengthFileFormats <- function(searchConditionsList = NULL, CreditCardTransactionFixedLengthFileFormatID = F, SkywardID = F, SkywardHash = F, CreditCardTransactionThirdPartyFormatID = F, NumberOfHeaderRows = F, TransactionImportIDStartPosition = F, SequenceNumberStartPosition = F, UniqueCardIdentifierStartPosition = F, TransactionDateStartPosition = F, TransactionAmountStartPosition = F, SplitAmountStartPosition = F, GeneralDefaultCodeOneStartPosition = F, GeneralDefaultCodeTwoStartPosition = F, GeneralDefaultCodeThreeStartPosition = F, GeneralDefaultCodeFourStartPosition = F, GeneralDefaultCodeFiveStartPosition = F, GeneralDefaultCodeSixStartPosition = F, GeneralDefaultCodeSevenStartPosition = F, GeneralDefaultCodeEightStartPosition = F, GeneralDefaultCodeNineStartPosition = F, GeneralDefaultCodeTenStartPosition = F, GeneralDefaultCodeElevenStartPosition = F, GeneralDefaultCodeTwelveStartPosition = F, VendorNameStartPosition = F, VendorCityStartPosition = F, VendorStateProvinceStartPosition = F, VendorZipCodeStartPosition = F, VendorCountryStartPosition = F, TransactionDescriptionStartPosition = F, SplitDescriptionStartPosition = F, TransactionImportIDLength = F, SequenceNumberLength = F, UniqueCardIdentifierLength = F, TransactionDateLength = F, TransactionAmountLength = F, SplitAmountLength = F, GeneralDefaultCodeOneLength = F, GeneralDefaultCodeTwoLength = F, GeneralDefaultCodeThreeLength = F, GeneralDefaultCodeFourLength = F, GeneralDefaultCodeFiveLength = F, GeneralDefaultCodeSixLength = F, GeneralDefaultCodeSevenLength = F, GeneralDefaultCodeEightLength = F, GeneralDefaultCodeNineLength = F, GeneralDefaultCodeTenLength = F, GeneralDefaultCodeElevenLength = F, GeneralDefaultCodeTwelveLength = F, VendorNameLength = F, VendorCityLength = F, VendorStateProvinceLength = F, VendorZipCodeLength = F, VendorCountryLength = F, TransactionDescriptionLength = F, SplitDescriptionLength = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UniqueAccountIdentifierStartPosition = F, UniqueAccountIdentifierLength = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionFixedLengthFileFormat", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCardTransactionThirdPartyFormats <- function(searchConditionsList = NULL, CreditCardTransactionThirdPartyFormatID = F, SkywardID = F, SkywardHash = F, SkywardIDClonedFrom = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, IsSystemLoaded = F, ImportType = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DefaultCreditCardTransactionDetailDescription = F, UniqueCardIdentifierType = F, UniqueAccountIdentifierType = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormat", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCardTransactionThirdPartyImports <- function(searchConditionsList = NULL, CreditCardTransactionThirdPartyImportID = F, CreditCardTransactionThirdPartyFormatID = F, ImportTime = F, ImportData = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyImport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCardTransactionThirdPartyFormatVendors <- function(searchConditionsList = NULL, CreditCardTransactionThirdPartyFormatVendorID = F, VendorID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CreditCardTransactionThirdPartyFormatID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormatVendor", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCreditCardTransactionDetails <- function(searchConditionsList = NULL, TempCreditCardTransactionDetailID = F, TempCreditCardTransactionID = F, CreditCardTransactionID = F, Description = F, EntryAmount = F, BaseCurrencyAmount = F, Quantity = F, UnitCost = F, AccountID = F, AccountDistribution = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasTax = F, SiteWAID = F, SiteCodeDescription = F, State = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempCreditCardTransactionDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsPayableTempErrors <- function(searchConditionsList = NULL, TempErrorID = F, TempCreditCardTransactionID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsPayableTempExceptions <- function(searchConditionsList = NULL, TempExceptionID = F, Message = F, LineNumber = F, IsFatal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listVendorProfileCalendarYearTotals <- function(searchConditionsList = NULL, CheckTransactionIDFirst = F, VendorID = F, DistrictID = F, CalendarYear = F, PaymentsAmountTotal = F, PaymentsAmount1099 = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "VendorProfileCalendarYearTotal", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempInvoiceAccountings <- function(searchConditionsList = NULL, TempInvoiceAccountingID = F, TempInvoiceID = F, AccountID = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceAccounting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPaymentTerms <- function(searchConditionsList = NULL, PaymentTermsID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DaysDue = F, BestDiscount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "PaymentTerms", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPaymentTermsDetails <- function(searchConditionsList = NULL, PaymentTermsDetailID = F, PaymentTermsID = F, DiscountDaysDue = F, DiscountPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "PaymentTermsDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAccountsPayableReports <- function(searchConditionsList = NULL, AccountsPayableReportID = F, DistrictID = F, ReportRunInfoID = F, DisplayOrder = F, Section = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "AccountsPayableReport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCreditCardTransactionThirdPartyFormatAccounts <- function(searchConditionsList = NULL, CreditCardTransactionThirdPartyFormatAccountID = F, CreditCardTransactionThirdPartyFormatID = F, AccountID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormatAccount", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}
