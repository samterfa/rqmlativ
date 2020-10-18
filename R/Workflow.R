

	listTempMassApproveErrors <- function(searchConditionsList = NULL, TempMassApproveErrorID = F, ErrorMessage = F, DataObjectID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ApprovalObjectDescription = F, Type = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TempMassApproveError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listWorkflowParameterMappings <- function(searchConditionsList = NULL, WorkflowParameterMappingID = F, ObjectWorkflowTriggerID = F, ParameterName = F, DataSource = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "WorkflowParameterMapping", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listObjectWorkflowTriggerSecurityGroups <- function(searchConditionsList = NULL, ObjectWorkflowTriggerSecurityGroupID = F, ObjectWorkflowTriggerID = F, SecurityGroupIDAssigned = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ObjectWorkflowTriggerSecurityGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCustomTasks <- function(searchConditionsList = NULL, CustomTaskID = F, CustomWorkflowID = F, Name = F, Description = F, Order = F, NextButtonText = F, TaskType = F, DataXML = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "CustomTask", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCustomTaskAssignedTos <- function(searchConditionsList = NULL, CustomTaskAssignedToID = F, CustomTaskID = F, UserIDAssignedTo = F, GroupIDSecurity = F, FieldIDToApplyUserTaskAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "CustomTaskAssignedTo", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCustomUITaskElements <- function(searchConditionsList = NULL, CustomUITaskElementID = F, CustomTaskID = F, ElementType = F, UITextValue = F, LinkTextValue = F, LinkHref = F, FieldID = F, Order = F, LimitTextField = F, TextFieldOptions = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "CustomUITaskElement", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCustomWorkflows <- function(searchConditionsList = NULL, CustomWorkflowID = F, Name = F, Description = F, Module = F, Object = F, ObjectIDRoot = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ScreenAttribute = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "CustomWorkflow", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTaskActionData <- function(searchConditionsList = NULL, TaskActionDataID = F, Order = F, ClassName = F, TaskID = F, Data = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TaskActionData", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listObjectWorkflowTriggers <- function(searchConditionsList = NULL, ObjectWorkflowTriggerID = F, WorkflowID = F, ObjectID = F, ObjectSaveMode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ObjectWorkflowTrigger", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTaskInstanceStatuses <- function(searchConditionsList = NULL, TaskInstanceStatusID = F, TaskInstanceID = F, CurrentRecordIdentifier = F, CurrentRecordNumber = F, TotalRecordCount = F, PercentComplete = F, ProcessName = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TaskInstanceStatus", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTaskConnections <- function(searchConditionsList = NULL, TaskConnectionID = F, TaskIDPrevious = F, TaskIDNext = F, IsSkywardTaskConnection = F, Condition = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TaskConnection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTaskInstanceUsers <- function(searchConditionsList = NULL, TaskInstanceUserID = F, UserID = F, TaskInstanceID = F, Type = F, TypeCode = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TaskInstanceUser", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTaskInstances <- function(searchConditionsList = NULL, TaskInstanceID = F, TaskID = F, WorkflowInstanceID = F, QueueCode = F, Queue = F, QueueTime = F, QueuedDuration = F, StatusCode = F, Status = F, StartTime = F, StopTime = F, ProcessDuration = F, Variables = F, PreviousWarningHashes = F, TaskActionXml = F, Application = F, Hostname = F, ThreadName = F, QueueDescription = F, ProcessID = F, UserIDPerformer = F, UserIDImpersonator = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LastActivity = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TaskInstance", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTasks <- function(searchConditionsList = NULL, TaskID = F, Name = F, Description = F, NavigationGroup = F, Queue = F, Type = F, TaskButtonCondition = F, AllowNextCondition = F, AllowNext = F, AllowPreviousCondition = F, AllowPrevious = F, AllowCancelCondition = F, AllowCancel = F, IsSkywardTask = F, ScreenXML = F, RemoveValidationContainerForUITask = F, WorkflowVersionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisableScreenValidationForUITask = F, ExpirationDurationOverrideCode = F, ExpirationDurationOverrideCode = F, ExpirationDurationOverrideCustomDays = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "Task", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listWorkflows <- function(searchConditionsList = NULL, WorkflowID = F, SkywardID = F, SkywardHash = F, Name = F, Module = F, Object = F, WorkflowVersionID = F, FullName = F, SearchableFullName = F, IsSkywardWorkflow = F, HasVersions = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "Workflow", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listWorkflowInstances <- function(searchConditionsList = NULL, WorkflowInstanceID = F, WorkflowVersionID = F, WorkflowInstanceIDRoot = F, LogID = F, StatusCode = F, Status = F, Success = F, ValidationFailureBehaviorCode = F, ValidationFailureBehaviorCode = F, UserIDImpersonator = F, EntityID = F, ScheduledTaskInstanceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AbortInitiated = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "WorkflowInstance", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listWorkflowInstanceVariables <- function(searchConditionsList = NULL, WorkflowInstanceVariableID = F, WorkflowInstanceID = F, ClassName = F, Name = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsEncrypted = F, SchemaObjectUniqueID = F, SchemaFieldUniqueID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "WorkflowInstanceVariable", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listWorkflowVersions <- function(searchConditionsList = NULL, WorkflowVersionID = F, SkywardHash = F, WorkflowID = F, Description = F, TaskID = F, Notes = F, Queue = F, Variables = F, ScreenAttribute = F, Portal = F, Parameters = F, RefreshScreen = F, Scope = F, AliasModule = F, AliasObject = F, AliasAction = F, FullAlias = F, NavigationGroups = F, IsSkywardWorkflowVersion = F, IsCurrentWorkflowVersion = F, HasWorkflowInstances = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, VariablesForClient = F, ExpirationDurationCode = F, ExpirationDurationCode = F, ExpirationDurationCustomDays = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "WorkflowVersion", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listWorkflowLocks <- function(searchConditionsList = NULL, WorkflowLockID = F, WorkflowInstanceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "WorkflowLock", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listApprovalTaskIdentifiers <- function(searchConditionsList = NULL, ApprovalTaskIdentifierID = F, TaskID = F, WorkflowVersionID = F, TaskType = F, TaskTypeCode = F, TypedApprovalTaskID = F, ApprovalType = F, ApprovalTypeCode = F, Level = F, Description = F, UseOrganizationChart = F, IsConditional = F, FilterData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ApprovalTaskIdentifier", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listApprovalTaskPresenters <- function(searchConditionsList = NULL, ApprovalTaskPresenterID = F, UserID = F, TaskInstanceID = F, Type = F, Description = F, ApprovalType = F, ApprovalTypeCode = F, ApprovalObjectID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ApprovalTaskPresenter", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listApprovalWorkflowLocks <- function(searchConditionsList = NULL, ApprovalWorkflowLockID = F, ApprovalType = F, ApprovalTypeCode = F, ApprovalObjectID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ApprovalWorkflowLock", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listApprovalTaskClearances <- function(searchConditionsList = NULL, ApprovalTaskClearanceID = F, ApprovalTaskIdentifierID = F, GroupIDSecurity = F, Type = F, TypeCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ApprovalTaskClearance", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listApprovalWorkflowInstances <- function(searchConditionsList = NULL, ApprovalWorkflowInstanceID = F, ApprovalType = F, ApprovalTypeCode = F, ApprovalObjectID = F, WorkflowInstanceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ApprovalWorkflowInstance", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTemplates <- function(searchConditionsList = NULL, TemplateID = F, Name = F, Description = F, DistrictID = F, EntityID = F, SchoolYearID = F, FiscalYearID = F, UserIDOwner = F, Published = F, Data = F, WorkflowID = F, TaskName = F, Controls = F, IsSkywardDefault = F, IsDefault = F, IsVisible = F, IsEditable = F, IsDeleteable = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsClonable = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "Template", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTemplateDefaults <- function(searchConditionsList = NULL, TemplateDefaultID = F, TemplateID = F, UserID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TemplateDefault", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTemplatePreviousValues <- function(searchConditionsList = NULL, TemplatePreviousValueID = F, TemplateID = F, UserID = F, Data = F, Controls = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TemplatePreviousValue", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTemplateLastUseds <- function(searchConditionsList = NULL, TemplateLastUsedID = F, TemplateID = F, UserID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TemplateLastUsed", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSynchronousLocks <- function(searchConditionsList = NULL, SynchronousLockID = F, WorkflowID = F, WorkflowInstanceID = F, LockingKey = F, Waiting = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "SynchronousLock", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listApprovalSupervisors <- function(searchConditionsList = NULL, ApprovalSupervisorID = F, OrganizationChartRelationshipID = F, ApprovalType = F, ApprovalTypeCode = F, ApprovalObjectID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ApprovalSupervisor", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempTemplateClones <- function(searchConditionsList = NULL, TempTemplateCloneID = F, Name = F, DistrictID = F, EntityID = F, SchoolYearID = F, FiscalYearID = F, District = F, Entity = F, SchoolYear = F, FiscalYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TempTemplateClone", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}
