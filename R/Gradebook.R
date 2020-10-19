

	listQuestionAnswerMedias <- function(searchConditionsList = NULL, QuestionAnswerMediaID = F, QuestionAnswerID = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayFor = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "QuestionAnswerMedia", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listQuestionMedias <- function(searchConditionsList = NULL, QuestionMediaID = F, QuestionID = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "QuestionMedia", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAnswers <- function(searchConditionsList = NULL, StudentAnswerID = F, StudentQuestionID = F, QuestionAnswerID = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentAnswer", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentQuestions <- function(searchConditionsList = NULL, StudentQuestionID = F, StudentAssignmentID = F, AssignmentQuestionID = F, Score = F, Order = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentQuestion", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentQuestions <- function(searchConditionsList = NULL, AssignmentQuestionID = F, AssignmentID = F, QuestionID = F, PointsAllowed = F, AllowPartialCredit = F, DisplayOrder = F, QuestionAnswerCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "AssignmentQuestion", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listClassGroupSections <- function(searchConditionsList = NULL, ClassGroupSectionID = F, ClassGroupID = F, SectionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ClassGroupSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listQuestions <- function(searchConditionsList = NULL, QuestionID = F, StaffID = F, Description = F, Instructions = F, Type = F, AllowStudentAttachments = F, UseAnswers = F, IsShortAnswer = F, IsEssay = F, IsMatching = F, IsMultipleChoice = F, IsTrueFalse = F, HasAutoScoredAssignment = F, HasAssignmentPastEndTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentCount = F, AssignmentCount = F, HasMultipleAssignments = F, HasQuestionMedias = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "Question", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listQuestionAnswers <- function(searchConditionsList = NULL, QuestionAnswerID = F, QuestionID = F, IsCorrect = F, Value = F, Choice = F, ChoiceOrder = F, ValueOrder = F, IsShortAnswer = F, IsEssay = F, IsTrueFalse = F, IsMultipleChoice = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasAttachedMedia = F, HasAttachedChoiceMedia = F, HasAttachedMediaSafe = F, HasAttachedChoiceMediaSafe = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "QuestionAnswer", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCalculationGroupSubjectAcademicStandards <- function(searchConditionsList = NULL, CalculationGroupSubjectAcademicStandardID = F, EntityGroupKey = F, CalculationGroupID = F, SubjectID = F, AcademicStandardID = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupSubjectAcademicStandardIDClonedFrom = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupSubjectAcademicStandard", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listClassGroups <- function(searchConditionsList = NULL, ClassGroupID = F, Name = F, EntityID = F, SchoolYearID = F, StaffID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAHistoricRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ClassGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionAcademicStandardWeights <- function(searchConditionsList = NULL, SectionAcademicStandardWeightID = F, SectionGradeBucketID = F, AcademicStandardID = F, WeightPercent = F, HasAssignments = F, AdjustedWeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionAcademicStandardWeight", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAcademicStandardGradingScaleGroups <- function(searchConditionsList = NULL, AcademicStandardGradingScaleGroupID = F, GradingScaleGroupID = F, CalculationGroupID = F, Description = F, EntityGroupKey = F, IsDefaultGroup = F, IsAHistoricRecord = F, StandardCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AcademicStandardGradingScaleGroupIDClonedFrom = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "AcademicStandardGradingScaleGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAcademicStandardGradingScaleGroupAcademicStandards <- function(searchConditionsList = NULL, AcademicStandardGradingScaleGroupAcademicStandardID = F, AcademicStandardGradingScaleGroupID = F, CalculationGroupAcademicStandardID = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AcademicStandardGradingScaleGroupAcademicStandardIDClonedFrom = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "AcademicStandardGradingScaleGroupAcademicStandard", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSubjectWeights <- function(searchConditionsList = NULL, SectionSubjectWeightID = F, SectionGradeBucketID = F, SubjectID = F, WeightPercent = F, HasAssignments = F, AdjustedWeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionSubjectWeight", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCalculationGroupAcademicStandardGradeBuckets <- function(searchConditionsList = NULL, CalculationGroupAcademicStandardGradeBucketID = F, CalculationGroupAcademicStandardID = F, CalculationGroupGradeBucketID = F, CalculationType = F, EntityGroupKey = F, IsAHistoricRecord = F, HasCategoryWeighting = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasAssignments = F, IsWeightedOnByACalculationGroupSubjectGradeBucket = F, IsWeightedOnInASectionOverride = F, CalculationGroupAcademicStandardGradeBucketIDClonedFrom = F, HasCalculationGroupWeight = F, HasCalculationGroupSubjectWeight = F, HasAcademicStandardWeighting = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupAcademicStandardGradeBucket", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCalculationGroupAcademicStandardWeightings <- function(searchConditionsList = NULL, CalculationGroupAcademicStandardWeightingID = F, CalculationGroupAcademicStandardGradeBucketID = F, CategoryIDToWeight = F, WeightPercent = F, IsAHistoricRecord = F, AdjustedWeightPercent = F, HasAssignments = F, PercentEarnedCategory = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityGroupKey = F, CalculationGroupAcademicStandardWeightingIDClonedFrom = F, AcademicStandardIDToWeight = F, UseForWeightSum = F, PointsEarnedCategory = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupAcademicStandardWeighting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCalculationGroupAcademicStandardWeightings <- function(searchConditionsList = NULL, TempCalculationGroupAcademicStandardWeightingID = F, CalculationGroupAcademicStandardGradeBucketID = F, CategoryID = F, CategoryDescription = F, WeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCalculationGroupAcademicStandardWeighting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCalculationGroupAcademicStandards <- function(searchConditionsList = NULL, CalculationGroupAcademicStandardID = F, CalculationGroupID = F, AcademicStandardID = F, EntityGroupKey = F, HasParentSubject = F, HasGradingScale = F, HasAssignments = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupAcademicStandardIDClonedFrom = F, Order = F, Level = F, CalculationGroupAcademicStandardIDParent = F, DisplayOnReportCard = F, CalculateForSingleBucket = F, IsDescendantOfSpecifiedAcademicStandard = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupAcademicStandard", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCalculationGroupSubjects <- function(searchConditionsList = NULL, CalculationGroupSubjectID = F, CalculationGroupID = F, SubjectID = F, EntityGroupKey = F, IsAHistoricRecord = F, HasGradingScale = F, HasAssignments = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupSubjectIDClonedFrom = F, Order = F, TopLevelAcademicStandardHierarchyDepthDescription = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupSubject", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCalculationGroupSubjectWeightings <- function(searchConditionsList = NULL, CalculationGroupSubjectWeightingID = F, CalculationGroupSubjectGradeBucketID = F, CategoryIDToWeight = F, AcademicStandardIDToWeight = F, WeightPercent = F, IsAHistoricRecord = F, AdjustedWeightPercent = F, HasAssignments = F, UseForWeightSum = F, PercentEarnedCategory = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityGroupKey = F, CalculationGroupSubjectWeightingIDClonedFrom = F, PointsEarnedCategory = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupSubjectWeighting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCalculationGroupSubjectWeightings <- function(searchConditionsList = NULL, TempCalculationGroupSubjectWeightingID = F, CalculationGroupSubjectGradeBucketID = F, CategoryID = F, CategoryDescription = F, WeightPercent = F, AcademicStandardID = F, AcademicStandardDescription = F, AcademicStandardFullKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCalculationGroupSubjectWeighting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCalculationGroupSubjectGradeBuckets <- function(searchConditionsList = NULL, CalculationGroupSubjectGradeBucketID = F, CalculationGroupSubjectID = F, CalculationGroupGradeBucketID = F, CalculationType = F, EntityGroupKey = F, IsAHistoricRecord = F, HasAssignments = F, HasCategoryWeighting = F, HasSubjectAcademicStandards = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupSubjectGradeBucketIDClonedFrom = F, HasCalculationGroupWeight = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupSubjectGradeBucket", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSubjectGradingScaleGroupSubjects <- function(searchConditionsList = NULL, SubjectGradingScaleGroupSubjectID = F, SubjectGradingScaleGroupID = F, CalculationGroupSubjectID = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubjectGradingScaleGroupSubjectIDClonedFrom = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SubjectGradingScaleGroupSubject", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSubjectGradingScaleGroups <- function(searchConditionsList = NULL, SubjectGradingScaleGroupID = F, GradingScaleGroupID = F, CalculationGroupID = F, Description = F, EntityGroupKey = F, IsDefaultGroup = F, IsAHistoricRecord = F, SubjectCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubjectGradingScaleGroupIDClonedFrom = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SubjectGradingScaleGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGradingScaleGroups <- function(searchConditionsList = NULL, GradingScaleGroupID = F, EntityID = F, SchoolYearID = F, Description = F, EntityGroupKey = F, MaxScore = F, GradingScaleType = F, IsDefaultSubjectGradingScaleGroup = F, IsDefaultAcademicStandardGradingScaleGroup = F, IsAHistoricRecord = F, HasSubjectGradingScaleGroups = F, HasAcademicStandardGradingScaleGroups = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayGradeMarkPercentages = F, GradingScaleGroupIDClonedFrom = F, SectionIDTeacherOverride = F, HasCourseGradingScaleGroups = F, HasSectionGradingScaleGroups = F, IsDummySectionContainer = F, IsSectionRelatedGradingScaleGroup = F, HasStudentGradingScaleGroups = F, IsPointsBasedScale = F, UseAsMastery = F, GradeMarkIDMastered = F, GradingScaleGroupExistsInSpecifcEntity = F, HasGradeMarkScoring = F, HasGradeMarksThatHaveChangedPercent = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "GradingScaleGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentGradeBucketSubjects <- function(searchConditionsList = NULL, StudentGradeBucketSubjectID = F, StudentGradeBucketID = F, SubjectID = F, SectionID = F, GradeMarkID = F, Percent = F, EarnedPoints = F, PossiblePoints = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAdminOverride = F, GradeMarkIDOverride = F, GradeMarkOverrideComment = F, PercentAdjustment = F, PercentAdjustmentComment = F, PercentWithAdjustment = F, GradeMarkToUse = F, PercentWithAdjustmentWithGradeMarkToUse = F, PercentWithGradeMark = F, CalculatedPoints = F, IsUsingPointsBasedScale = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGradeBucketSubject", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTeacherSectionStudentSectionSettings <- function(searchConditionsList = NULL, TeacherSectionStudentSectionSettingID = F, TeacherSectionSettingID = F, StudentSectionID = F, DisplayOrder = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionStudentSectionSetting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGradebookTempAssignmentErrors <- function(searchConditionsList = NULL, TempAssignmentErrorID = F, ErrorText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempSectionAssignmentID = F, ErrorNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempAssignmentError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTeacherSectionStandardsDisplayGradeBucketSettings <- function(searchConditionsList = NULL, TeacherSectionStandardsDisplayGradeBucketSettingID = F, TeacherSectionSettingID = F, CalculationGroupGradeBucketID = F, ShowBucket = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionStandardsDisplayGradeBucketSetting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempAdjustedCategories <- function(searchConditionsList = NULL, TempAdjustedCategoryID = F, TempSectionCategoryID = F, CategoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempAdjustedCategory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStudentAssignments <- function(searchConditionsList = NULL, TempStudentAssignmentID = F, StudentSectionID = F, AssignmentID = F, SectionID = F, Score = F, MaxScore = F, Weight = F, AssignmentName = F, AssignmentDescription = F, DueDate = F, Include = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsMissing = F, NoCount = F, ScoreClarifierID = F, ScoreClarifierCode = F, DueDateIsInGivenDateRange = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempStudentAssignment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempGradingPeriodGradeBucketErrors <- function(searchConditionsList = NULL, TempGradingPeriodGradeBucketErrorID = F, ErrorText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubjectOrStandardDescription = F, SubjectOrStandardKey = F, CalculationType = F, GradingPeriodGradeBucketID = F, ErrorNumber = F, GradeBucketLabelWithDates = F, StartDate = F, EndDate = F, StatusDisplayWithExtensionDate = F, SectionLengthCodeDescription = F, CalculationGroupID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempGradingPeriodGradeBucketError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTeacherSectionStandardsDisplayAcademicStandardSettings <- function(searchConditionsList = NULL, TeacherSectionStandardsDisplayAcademicStandardSettingID = F, AcademicStandardID = F, TeacherSectionStandardsDisplayGradeBucketSettingID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionStandardsDisplayAcademicStandardSetting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentGradeBucketAcademicStandards <- function(searchConditionsList = NULL, StudentGradeBucketAcademicStandardID = F, StudentGradeBucketID = F, AcademicStandardID = F, GradeMarkID = F, Percent = F, EarnedPoints = F, PossiblePoints = F, SectionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAdminOverride = F, GradeMarkIDOverride = F, GradeMarkOverrideComment = F, PercentAdjustment = F, PercentAdjustmentComment = F, PercentWithAdjustment = F, PercentWithAdjustmentWithGradeMarkToUse = F, PercentWithGradeMark = F, GradeMarkToUse = F, CalculatedPoints = F, IsUsingPointsBasedScale = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGradeBucketAcademicStandard", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCourseMoveCourses <- function(searchConditionsList = NULL, TempCourseMoveCourseID = F, CourseID = F, CourseCode = F, CourseDescription = F, OriginalCalculationGroupID = F, OriginalCalculationGroupDescription = F, NewCalculationGroupID = F, NewCalculationGroupDescription = F, IsValidUpdate = F, EntityID = F, SchoolYearID = F, GroupCourseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CodeDescription = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCourseMoveCourse", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCourseMoveErrors <- function(searchConditionsList = NULL, TempCourseMoveErrorID = F, SectionID = F, BucketLabel = F, ErrorCode = F, ErrorMessage = F, ErrorType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCourseMoveError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCourseMoveSectionErrors <- function(searchConditionsList = NULL, TempCourseMoveSectionErrorID = F, CourseID = F, SectionID = F, SectionCode = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCourseMoveSectionError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempGradeMarkErrors <- function(searchConditionsList = NULL, TempGradeMarkErrorID = F, GradeMarkID = F, GradeMarkCode = F, SectionID = F, DisplayOrder = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempGradeMarkError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempSectionGradingScaleGroupToCreates <- function(searchConditionsList = NULL, TempSectionGradingScaleGroupToCreateID = F, CourseGradingScaleGroupID = F, CourseGradingScaleGroupCourseID = F, NewCourseGradingScaleGroupID = F, NewCourseGradingScaleGroupDescription = F, SectionID = F, CourseID = F, SectionCode = F, Description = F, RequiresSectionGradingScaleGroups = F, Reason = F, GradingPeriodGradeBucketsToRetainJson = F, IsValidInNewGroup = F, SectionLengthCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseCode = F, InvalidGradeMarks = F, CourseDescriptionCodeSectionCode = F, StaffFullNameLFM = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempSectionGradingScaleGroupToCreate", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTeacherSectionCategoryAnalyticsSettings <- function(searchConditionsList = NULL, TeacherSectionCategoryAnalyticsSettingID = F, CategoryID = F, TeacherSectionSettingID = F, AnalysisType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionCategoryAnalyticsSetting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssessments <- function(searchConditionsList = NULL, AssessmentID = F, SectionID = F, CategoryID = F, AssignedDate = F, DueDate = F, Name = F, Description = F, IsDeleted = F, CanDelete = F, DisplayRestoreButton = F, AssignmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "Assessment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCategoryGradeBucketTypes <- function(searchConditionsList = NULL, CategoryGradeBucketTypeID = F, CategoryID = F, GradeBucketTypeID = F, EntityGroupKey = F, CategoryGradeBucketTypeIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CategoryGradeBucketType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGradebookConfigEntityGroupYears <- function(searchConditionsList = NULL, ConfigEntityGroupYearID = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, GradingPeriodEditExtensionDays = F, GradingPeriodEditExtensionEndTime = F, CapWeightedCategoryCalculationAt100Percent = F, AllowRetainedFutureScoring = F, OnlyShowGradebooksWithActiveStudentSectionTransactions = F, ClosedGradingPeriodGradeChangePermissionType = F, ClosedGradingPeriodGradeChangeCutOff = F, AllowPercentAdjustment = F, NumberOfDaysUntilUnscoredAssignmentsAreMissing = F, NumberOfDaysUntilNewStudentIconAppears = F, AssignmentMaxScore = F, AllowTeachersToOverrideDefaultMaxScore = F, AssignmentMaxWeight = F, AllowTeachersToOverrideDefaultMaxWeight = F, AllowTeacherComments = F, CapCalculationAt100Percent = F, ConfigEntityGroupYearIDClonedFrom = F, DisplayNonGradedClassesForTeachers = F, AllowOnlineAssignments = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AutoApproveGradeChangeRequest = F, AllowTeachersToTransferGrades = F, AllowTeachersToAddStudentNotes = F, UseFailingGradeScoreClarifier = F, FailingGradeThresholdPercent = F, ScoreClarifierIDFailing = F, AllowNegativePercentAdjustment = F, AllowGracePeriodAtEndOfGradingPeriod = F, AllowMultipleAssignmentAttempts = F, AllowStudentGroups = F, AllowClosedGradingPeriodGradeChangeRequestsToUpdateSnapshotGrades = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ConfigEntityGroupYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listClosedGradingPeriodStudentGradeBucketChanges <- function(searchConditionsList = NULL, ClosedGradingPeriodStudentGradeBucketChangeID = F, ClosedGradingPeriodGradeChangeID = F, StaffMeetID = F, StudentGradeBucketID = F, GradeMarkIDOriginal = F, GradeMarkIDNew = F, ClosedGradingPeriodGradeChangeStatus = F, IsDisabled = F, IsGradeChangePastDueAndInProgress = F, CalculatedClosedGradingPeriodGradeChangeStatus = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OriginalPercent = F, NewPercent = F, HasSnapshot = F, ChangeMadeByFullnameLFM = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ClosedGradingPeriodStudentGradeBucketChange", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listClosedGradingPeriodGradeChanges <- function(searchConditionsList = NULL, ClosedGradingPeriodGradeChangeID = F, SectionID = F, GradingPeriodID = F, CompletedTime = F, Reason = F, DisplayCompleteButton = F, CalculatedCompletedTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayMassReviewDenyButton = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ClosedGradingPeriodGradeChange", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTeacherSectionGradeBucketAnalyticsSettings <- function(searchConditionsList = NULL, TeacherSectionGradeBucketAnalyticsSettingID = F, CalculationGroupGradeBucketID = F, TeacherSectionSettingID = F, AnalysisType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionGradeBucketAnalyticsSetting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentSectionNotes <- function(searchConditionsList = NULL, StudentSectionNoteID = F, StudentSectionID = F, Note = F, OtherTeachersHaveAccess = F, LimitToThisSection = F, CurrentUserCanModify = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentSectionNote", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionGradeBucketSettings <- function(searchConditionsList = NULL, SectionGradeBucketSettingID = F, SectionID = F, GradingPeriodGradeBucketID = F, UseMaxExtraCredit = F, MaxExtraCredit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionGradeBucketSetting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionGradingPeriodData <- function(searchConditionsList = NULL, SectionGradingPeriodDataID = F, SectionID = F, GradingPeriodID = F, IsCompleted = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionGradingPeriodData", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTeacherSectionGradingPeriodSettings <- function(searchConditionsList = NULL, TeacherSectionGradingPeriodSettingID = F, TeacherSectionSettingID = F, GradingPeriodID = F, ShowGradeBuckets = F, ShowAssignments = F, ShowAssessments = F, IncludeMissingAssignments = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionGradingPeriodSetting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTeacherSectionGradeBucketSettings <- function(searchConditionsList = NULL, TeacherSectionGradeBucketSettingID = F, TeacherSectionSettingID = F, GradingPeriodGradeBucketID = F, GradeBucketDisplayType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsExpanded = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionGradeBucketSetting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTeacherSectionSettings <- function(searchConditionsList = NULL, TeacherSectionSettingID = F, SectionID = F, ClassGroupID = F, UserIDOwner = F, ManualDateEntryStartDate = F, ManualDateEntryEndDate = F, StudentsToDisplay = F, StudentNameDisplayType = F, DisplayStudentGradeLevel = F, DisplayStudentNumber = F, DisplayAttendance = F, DisplayMissingAssignment = F, DisplayUnscoredPastDueAssignments = F, DisplayGradebookAverages = F, DisplayAssignmentAttendanceIndicator = F, DisplayStudentGradingPeriodCommentIndicator = F, CalculationGroupGradeBucketIDLocked = F, BrowseViewID = F, HideLockedColumns = F, ShowGradebookLockedColumnButton = F, UseCustomClassRosterSort = F, HasCustomClassRosterSort = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HideAssignmentCategoryColors = F, DisplayStudentGroups = F, DisplayStudentGroupColors = F, DisplayAssignmentStudentGroupColors = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionSetting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGradesheetAssignmentSettings <- function(searchConditionsList = NULL, GradesheetAssignmentSettingID = F, TeacherSectionSettingID = F, MaxScoreDefault = F, IsDateAscending = F, SortBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DefaultAttemptType = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "GradesheetAssignmentSetting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempSectionGradingScales <- function(searchConditionsList = NULL, TempSectionGradingScaleID = F, SectionID = F, CanClone = F, ErrorMessage = F, ChildGradeBuckets = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempSectionGradingScale", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCloneGradebookSections <- function(searchConditionsList = NULL, TempCloneGradebookSectionID = F, SectionID = F, CanClone = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCloneGradebookSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCalculationGroupWeightings <- function(searchConditionsList = NULL, TempCalculationGroupWeightingID = F, CalculationGroupGradeBucketID = F, GradingPeriodGradeBucketID = F, CategoryID = F, CategoryDescription = F, GradeBucketLabel = F, WeightPercent = F, GradeBucketOrder = F, SubjectID = F, SubjectDescription = F, AcademicStandardID = F, AcademicStandardDescription = F, AcademicStandardFullKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCalculationGroupWeighting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempGradebookCloneErrors <- function(searchConditionsList = NULL, TempGradebookCloneErrorID = F, CalculationGroupGradeBucketID = F, SectionID = F, Field = F, Message = F, GradeBucketLabel = F, RecordType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempGradebookCloneError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempSectionGradeBucketWeights <- function(searchConditionsList = NULL, TempSectionGradeBucketWeightID = F, CalculationGroupGradeBucketIDFeeder = F, CalculationGroupGradeBucketIDComposite = F, SectionID = F, WeightPercent = F, CanClone = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AcademicStandardIDToWeight = F, SubjectIDToWeight = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempSectionGradeBucketWeight", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempSectionCategories <- function(searchConditionsList = NULL, TempSectionCategoryID = F, CalculationGroupGradeBucketID = F, CalculationGroupID = F, SectionID = F, CategoryID = F, CategoryCode = F, WeightPercent = F, CanClone = F, ErrorMessages = F, CategoryIDIsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempSectionCategory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionGradeBucketWeights <- function(searchConditionsList = NULL, SectionGradeBucketWeightID = F, SectionGradeBucketIDFeeder = F, SectionGradeBucketIDComposite = F, WeightPercent = F, RoundBucketPercent = F, AdjustedWeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionGradeBucketWeight", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionCategories <- function(searchConditionsList = NULL, SectionCategoryID = F, SectionGradeBucketID = F, CategoryID = F, WeightPercent = F, PercentEarnedForCategory = F, HasAssignments = F, AdjustedWeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PointsEarnedForCategory = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionCategory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentSectionGradingScaleGradeBuckets <- function(searchConditionsList = NULL, StudentSectionGradingScaleGradeBucketID = F, GradingPeriodGradeBucketID = F, StudentGradingScaleGroupStudentSectionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentSectionGradingScaleGradeBucket", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempSectionGradeBuckets <- function(searchConditionsList = NULL, TempSectionGradeBucketID = F, SectionGradeBucketID = F, CalculationGroupGradeBucketID = F, GradeBucketTypeID = F, IsSpecificGradeBucket = F, SectionID = F, GradeBucketLabel = F, CanClone = F, HasLeftOverPercentage = F, Exclude = F, ErrorMessages = F, StartDate = F, EndDate = F, ChildRecords = F, IsCalculationTypeOverridden = F, CurrentAllowCategoryOverride = F, CurrentCalculationType = F, NewCalculationType = F, Order = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempSectionGradeBucket", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempDropLowScoreStudentAssignments <- function(searchConditionsList = NULL, TempDropLowScoreStudentAssignmentID = F, StudentAssignmentID = F, StudentName = F, StudentSectionID = F, AssignmentName = F, DropCode = F, DueDate = F, MaxScore = F, Weight = F, DropLowScore = F, NoDropReason = F, Comments = F, Score = F, OriginalGrade = F, NewGrade = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentDisplaySortCode = F, DropActionType = F, UndoDropActionType = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempDropLowScoreStudentAssignment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGradebookAssignments <- function(searchConditionsList = NULL, AssignmentID = F, SectionID = F, CategoryID = F, AssignedDate = F, DueDate = F, Name = F, Description = F, MaxScore = F, Weight = F, ScoreDisplayType = F, AssignmentSyncKey = F, IsDeleted = F, IsOnlineAssignment = F, Instructions = F, StartTime = F, EndTime = F, DefaultPointsPerQuestion = F, RandomizeQuestions = F, AutoScore = F, ShowCorrectAnswers = F, AnswerRevealTime = F, AssessmentID = F, AcademicStandardID = F, SubjectID = F, CanDelete = F, DisplayRestoreButton = F, UseGradeMarkScoring = F, ScoredStudentAssignmentCount = F, ActiveStudentCount = F, HasUngradedStudentAssignments = F, HasQuestions = F, QuestionCount = F, AveragePercent = F, DueDateInCurrentOpenExtendedGradingPeriod = F, IsAHistoricRecord = F, DueDateIsInGivenDateRange = F, IsPastEndTime = F, AssignmentQuestionCount = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowAutoScore = F, CalculatedPointsAllowedFromQuestions = F, SendNotificationWhenAssignmentReadyToTake = F, SendNotificationWhenAnswersRevealed = F, HideScoreUntilTime = F, HasStudentAssignments = F, HasStudentAssignmentsWithScoreOrGradeMark = F, AllStudentAssignmentsAreNotStarted = F, HasStudentFamilyAttachmentsToDisplay = F, StudentFamilyAccessAttachmentCount = F, ShowScore = F, HasWholeNumberWeight = F, AssignmentIDParent = F, IsParent = F, HasParent = F, DueDateInTransactionForGivenStudentSection = F, RelatedAssignmentsHaveAcademicStandards = F, RelatedAssignmentsHaveSubjects = F, ChildAssignmentCount = F, Subjects = F, AcademicStandards = F, IsActiveOrUnlocked = F, HasStudentGroupAssignments = F, ActiveStudentGroups = F, ClassesSyncedTo = F, IsAvailableForStudentSection = F, IsAvailableForStudentGroup = F, CreateStudentGroupAssignmentErrorMessage = F, IsSynced = F, EffectiveAcademicStandardID = F, AssignmentIDClonedFrom = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "Assignment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGradebookCategories <- function(searchConditionsList = NULL, CategoryID = F, Code = F, Description = F, DistrictID = F, SchoolYearID = F, DistrictGroupKey = F, CodeDescription = F, BackgroundColor = F, TextColor = F, UseForSpecificGradeBucketType = F, CategoryIDClonedFrom = F, AssignmentCount = F, GradeBucketTypeCategory = F, DescriptionWithCategoryWeightPercent = F, CategoryUsedInTotalPointsDefaultCalculationSetup = F, CategoryUsedInCategoryWeightingDefaultSetup = F, CalculationGroupAllowCategoryOverride = F, HasAssignments = F, AverageStudentAssignmentScoreForCategoryAndStudentSection = F, IsValidInCalculationGroup = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasSpecificCalculationGroupWeight = F, HasSpecificCalculationGroupSubjectWeight = F, HasSpecificCalculationGroupAcademicStandardWeight = F, WeightingAllowedForGradeBucketType = F, IsAHistoricRecord = F, EdFiGradebookEntryTypeDescriptorID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "Category", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCalculationGroupCategories <- function(searchConditionsList = NULL, CalculationGroupCategoryID = F, CalculationGroupID = F, CategoryID = F, EntityGroupKey = F, HasAssignments = F, CalculationGroupCategoryIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupCategory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCalculationGroupCourses <- function(searchConditionsList = NULL, CalculationGroupCourseID = F, CalculationGroupID = F, CourseID = F, CalculationGroupCourseIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupCourse", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCalculationGroupGradeBuckets <- function(searchConditionsList = NULL, CalculationGroupGradeBucketID = F, CalculationGroupID = F, GradingPeriodGradeBucketID = F, AllowCalculationTypeOverride = F, AllowWeightOverride = F, CalculationType = F, CalculationGroupGradeBucketIDClonedFrom = F, RoundBucketPercent = F, GradeMarkScoringType = F, IsAHistoricRecord = F, HasCategoryWeighting = F, HasGradeBucketWeighting = F, HasSubjectWeighting = F, HasAcademicStandardWeighting = F, WeightPercentTotal = F, InnerCalculationGroupGradeBucketsCount = F, EffectiveCalculationTypeCode = F, NumberOfCategories = F, HasSubjects = F, HasAcademicStandards = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityGroupKey = F, CourseCount = F, InUseBySections = F, IsMissingStudentGradeBucketAcademicStandards = F, IsMissingStudentGradeBucketSubjects = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupGradeBucket", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCalculationGroupWeightings <- function(searchConditionsList = NULL, CalculationGroupWeightingID = F, CalculationGroupGradeBucketID = F, CategoryIDToWeight = F, GradingPeriodGradeBucketIDToWeight = F, SubjectIDToWeight = F, AcademicStandardIDToWeight = F, WeightPercent = F, RoundBucketPercent = F, RequiredGrade = F, CalculationGroupWeightingIDClonedFrom = F, PercentEarnedForCategory = F, IsAHistoricRecord = F, AdjustedGradeBucketWeightPercent = F, AdjustedCategoryWeightPercent = F, AdjustedSubjectWeightPercent = F, AdjustedAcademicStandardWeightPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityGroupKey = F, PointsEarnedForCategory = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupWeighting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseGradingScaleGroupCourses <- function(searchConditionsList = NULL, CourseGradingScaleGroupCourseID = F, CourseGradingScaleGroupID = F, CourseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CourseGradingScaleGroupCourse", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listScoreClarifiers <- function(searchConditionsList = NULL, ScoreClarifierID = F, Code = F, Description = F, IsMissing = F, NoCount = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, ScoreClarifierIDClonedFrom = F, CodeDescription = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ScoreClarifier", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionGradeBuckets <- function(searchConditionsList = NULL, SectionGradeBucketID = F, SectionID = F, CalculationGroupGradeBucketID = F, IsCalculationTypeOverridden = F, IsOverridden = F, EffectiveCalculationTypeCode = F, HasSectionCategories = F, HasSectionGradeBucketWeights = F, IsAHistoricRecord = F, HasSectionSubjectWeights = F, HasSectionAcademicStandardWeights = F, CalculationModifiedTime = F, CalculationType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasWeightOverride = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionGradeBucket", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionGradingScaleGroups <- function(searchConditionsList = NULL, SectionGradingScaleGroupID = F, SectionID = F, GradingPeriodGradeBucketID = F, SectionGradingScaleGroupIDClonedFrom = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GradingScaleGroupID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "SectionGradingScaleGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCalculationGroups <- function(searchConditionsList = NULL, CalculationGroupID = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, Description = F, Filter = F, AllowCategoryOverride = F, Rank = F, IsDefaultGroup = F, CalculationGroupIDClonedFrom = F, LimitTeacherOverridesToCategoriesInGroup = F, IsAHistoricRecord = F, HasSubjects = F, HasAcademicStandards = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowAssignmentPointsTeacherOverride = F, AllowCategoryWeightingTeacherOverride = F, AllowGradeBucketWeightingTeacherOverride = F, AllowTotalPointsAndGradeBucketWeightingTeacherOverride = F, AllowNotGradedTeacherOverride = F, AllowSubjectiveTeacherOverride = F, AllowSubjectWeightingTeacherOverride = F, AllowAcademicStandardWeightingTeacherOverride = F, HasAssignmentPointsOverrides = F, HasCategoryWeightingOverrides = F, HasGradeBucketWeightingOverrides = F, HasTotalPointsAndGradeBucketWeightingOverrides = F, HasNotGradedOverrides = F, HasSubjectiveOverrides = F, HasSubjectWeightingOverrides = F, HasAcademicStandardWeightingOverrides = F, AllowDecayingAverageTeacherOverride = F, DecayingAveragePercent = F, HasDecayingAverageOverrides = F, HasDecayingAverage = F, AllowAssignmentAveragePercentTeacherOverride = F, HasAssignmentAveragePercentOverrides = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseGradingScaleGroups <- function(searchConditionsList = NULL, CourseGradingScaleGroupID = F, EntityID = F, SchoolYearID = F, Filter = F, Description = F, EntityGroupKey = F, Rank = F, AllowSectionOverride = F, IsDefaultGroup = F, CourseGradingScaleGroupIDClonedFrom = F, LimitTeacherOverridesToGradeMarksInGroup = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GradingScaleGroupID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CourseGradingScaleGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGradingScaleGroupGradeMarks <- function(searchConditionsList = NULL, GradingScaleGroupGradeMarkID = F, GradeMarkID = F, PercentLow = F, PercentHigh = F, DefaultCalculationPercent = F, Color = F, GradingScaleGroupID = F, GradingScaleGroupGradeMarkIDClonedFrom = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowSubjective = F, PointsLow = F, PointsHigh = F, DefaultCalculationPoints = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "GradingScaleGroupGradeMark", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAssignments <- function(searchConditionsList = NULL, StudentAssignmentID = F, StudentSectionID = F, AssignmentID = F, ScoreClarifierID = F, Score = F, Comments = F, IsMissing = F, IsGraded = F, NoCount = F, IsDropped = F, SubmissionStatus = F, SubmissionTime = F, SectionID = F, GradeMarkID = F, PointsEarned = F, PointsEarnedWithSlash = F, PointsEarnedPercent = F, ScoreDisplayValue = F, AllQuestionsScored = F, IsExpired = F, ScoreDisplayValueNoGradeMark = F, IsTransferredScore = F, IsFutureRetainedScore = F, IsScored = F, HasStudentSectionTransaction = F, AssignmentDueDateAttendance = F, StudentQuestionCount = F, HasAnswers = F, AnsweredQuestionCount = F, CurrentQuestionNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentOnlineAssignmentDisplayPointsEarnedWithSlash = F, GradeMarkCode = F, OnlineAssignmentNotificationSent = F, OnlineAssignmentReviewNotificationSent = F, AnswerKeyIsAvailableToView = F, PointsEarnedWithSlashCheckDisplay = F, SubmissionStatusToUse = F, CannotBeTakenByStudent = F, CalculatedPoints = F, ScoreHasChangedWithinSpecifiedAmountOfTime = F, StudentOnlineAssignmentDisplayPointsEarned = F, AttemptCount = F, TimeLastScored = F, IsMissingHasChangedWithinSpecifiedAmountOfTime = F, GradeMarkCodeToUse = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentAssignment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentGradingScaleGroups <- function(searchConditionsList = NULL, StudentGradingScaleGroupID = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, Description = F, StudentGradingScaleGroupIDClonedFrom = F, StudentGradingScaleGroupStudentSectionCount = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GradingScaleGroupID = F, HasAttachedStudentSections = F, AllowTeachersToModify = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGradingScaleGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentGradingScaleGroupStudentSections <- function(searchConditionsList = NULL, StudentGradingScaleGroupStudentSectionID = F, StudentGradingScaleGroupID = F, StudentSectionID = F, GradeBuckets = F, IsAHistoricRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGradingScaleGroupStudentSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCalcGroupBucketRemoveStandardOrSubjectResults <- function(searchConditionsList = NULL, TempCalcGroupBucketRemoveStandardOrSubjectResultID = F, IsError = F, StandardOrSubjectDescription = F, ErrorText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempCalcGroupBucketRemoveStandardOrSubjectResult", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTeacherSectionAcademicStandardGradeBucketSettings <- function(searchConditionsList = NULL, TeacherSectionAcademicStandardGradeBucketSettingID = F, TeacherSectionSettingID = F, GradingPeriodGradeBucketID = F, AcademicStandardID = F, IsExpanded = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Display = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionAcademicStandardGradeBucketSetting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTeacherSectionSubjectGradeBucketSettings <- function(searchConditionsList = NULL, TeacherSectionSubjectGradeBucketSettingID = F, TeacherSectionSettingID = F, GradingPeriodGradeBucketID = F, SubjectID = F, IsExpanded = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Display = F, AllLinkedAcademicStandardsAreDisplayed = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TeacherSectionSubjectGradeBucketSetting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMonitorSummaryByClasses <- function(searchConditionsList = NULL, MonitorSummaryByClassID = F, LastUpdatedTime = F, StaffMeetID = F, ClosedGradingPeriodGradeChangeCount = F, AssignmentCountYTD = F, StudentCountForTerm = F, AssignmentCountForTerm = F, FutureAssignmentCountForTerm = F, GradedAssignmentCountForTerm = F, NonGradedAssignmentCountForTerm = F, MissingAssignmentCountForTerm = F, NoCountAssignmentCountForTerm = F, ScoreClarifierAssignmentCountForTerm = F, ScoredAssignmentRange100to90CurrentTerm = F, ScoredAssignmentRange89to80CurrentTerm = F, ScoredAssignmentRange79to70CurrentTerm = F, ScoredAssignmentRange69to60CurrentTerm = F, ScoredAssignmentRange59to50CurrentTerm = F, ScoredAssignmentRange49to1CurrentTerm = F, ScoredAssignmentRange0CurrentTerm = F, ExcusedAbsencesYTD = F, UnexcusedAbsencesYTD = F, TardiesYTD = F, OtherAbsencesYTD = F, OffenseCountYTD = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentGradingPeriod = F, LastAccessedGradebookTime = F, LastScoredGradebookTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "MonitorSummaryByClass", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMonitorSummaryByTeachers <- function(searchConditionsList = NULL, MonitorSummaryByTeacherID = F, LastUpdatedTime = F, StaffID = F, StaffMeetCount = F, ActiveStudentCount = F, AssignmentCountForTerm = F, NonGradedAssignmentCountNoStudentAssignmentsForTerm = F, NonGradedAssignmentCountForTerm = F, GradedAssignmentCountForTerm = F, NoCountAssignmentCountForTerm = F, MissingAssignmentCountForTerm = F, ScoreClarifierAssignmentCountForTerm = F, FutureAssignmentCountForTerm = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SchoolYearID = F, LastAccessedGradebookTime = F, LastAssignmentScoredDueDate = F, LastScoredAssignmentTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "MonitorSummaryByTeacher", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentAttachments <- function(searchConditionsList = NULL, AssignmentAttachmentID = F, AssignmentID = F, AttachmentID = F, DisplayInStudentFamilyAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "AssignmentAttachment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGradebookConfigEntities <- function(searchConditionsList = NULL, ConfigEntityID = F, EntityID = F, RunInMonitorByScheduledTask = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ConfigEntity", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGradebookConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ConfigDistrict", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempSectionAssignments <- function(searchConditionsList = NULL, TempSectionAssignmentID = F, AssignmentID = F, AssignmentName = F, SectionID = F, SectionDetails = F, CurrentPeriod = F, Sync = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CategoryDescription = F, EntityID = F, SchoolYearID = F, AssignedDate = F, DueDate = F, SectionLengthStartDate = F, SectionLengthEndDate = F, MaxScore = F, Weight = F, IsOnlineAssignment = F, StartDate = F, StartTime = F, EndDate = F, EndTime = F, HideScoreUntilDate = F, HideScoreUntilTime = F, ShowCorrectAnswers = F, AnswerRevealDate = F, AnswerRevealTime = F, AttachmentCount = F, CategoryID = F, UseForSpecificGradeBucketType = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempSectionAssignment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDropLowScoreRunStudentAssignments <- function(searchConditionsList = NULL, DropLowScoreRunStudentAssignmentID = F, DropLowScoreRunID = F, StudentAssignmentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DropActionType = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "DropLowScoreRunStudentAssignment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDropLowScoreRuns <- function(searchConditionsList = NULL, DropLowScoreRunID = F, RunTime = F, IsEffective = F, SectionID = F, CalculationGroupGradeBucketID = F, AffectedStudentAssignmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "DropLowScoreRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempGradingPeriodGradeBuckets <- function(searchConditionsList = NULL, TempGradingPeriodGradeBucketID = F, GradingPeriodGradeBucketID = F, GradeBucketLabelWithDates = F, StartDate = F, EndDate = F, StatusDisplayWithExtensionDate = F, SectionLengthCodeDescription = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempGradingPeriodGradeBucket", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempUnDropLowScoreStudentSections <- function(searchConditionsList = NULL, TempUnDropLowScoreStudentSectionID = F, StudentName = F, StudentDisplaySortCode = F, StudentSectionID = F, OriginalGrade = F, NewGrade = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempUnDropLowScoreStudentSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCalculationGroupAcademicStandardCalculationGroupHierarchyDepths <- function(searchConditionsList = NULL, CalculationGroupAcademicStandardCalculationGroupHierarchyDepthID = F, EntityGroupKey = F, CalculationGroupAcademicStandardID = F, CalculationGroupHierarchyDepthID = F, CalculationGroupAcademicStandardIDAtLevel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupAcademicStandardCalculationGroupHierarchyDepthIDClonedFrom = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupAcademicStandardCalculationGroupHierarchyDepth", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCalculationGroupHierarchyDepths <- function(searchConditionsList = NULL, CalculationGroupHierarchyDepthID = F, EntityGroupKey = F, CalculationGroupID = F, Code = F, Description = F, DepthLevel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalculationGroupHierarchyDepthIDClonedFrom = F, DynamicRelationshipID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "CalculationGroupHierarchyDepth", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempGradebookGroupErrors <- function(searchConditionsList = NULL, TempGradebookGroupErrorID = F, EntityID = F, CalculationGroupID = F, GradingScaleGroupID = F, Description = F, ErrorText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempGradebookGroupError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAssignmentAttempts <- function(searchConditionsList = NULL, StudentAssignmentAttemptID = F, StudentAssignmentID = F, Score = F, Comments = F, IsUsed = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GradeMarkID = F, DateAttempted = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentAssignmentAttempt", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentGroupTeacherSectionSettings <- function(searchConditionsList = NULL, StudentGroupTeacherSectionSettingID = F, StudentGroupID = F, TeacherSectionSettingID = F, Display = F, Color = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGroupTeacherSectionSetting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentGroupAssignments <- function(searchConditionsList = NULL, StudentGroupAssignmentID = F, StudentGroupID = F, AssignmentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DeleteErrorMessage = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGroupAssignment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentGroupStudentSections <- function(searchConditionsList = NULL, StudentGroupStudentSectionID = F, StudentGroupID = F, StudentSectionID = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AssignmentCount = F, ScoredAssignmentCount = F, IsActive = F, CanBeDeleted = F, UnableToDeleteMessage = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGroupStudentSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentGroups <- function(searchConditionsList = NULL, StudentGroupID = F, SectionID = F, Name = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, AssignmentCount = F, StudentCount = F, CanBeDeleted = F, StudentGroupSyncKey = F, IsAHistoricRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGradebookVendorCategories <- function(searchConditionsList = NULL, VendorCategoryID = F, CategoryID = F, OneRosterVendorID = F, VendorSourceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "VendorCategory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listVendorStudentAssignments <- function(searchConditionsList = NULL, VendorStudentAssignmentID = F, StudentAssignmentID = F, OneRosterVendorID = F, VendorSourceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "VendorStudentAssignment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listVendorAssignments <- function(searchConditionsList = NULL, VendorAssignmentID = F, AssignmentID = F, OneRosterVendorID = F, VendorSourceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "VendorAssignment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStudentGradingScaleGroupStudentSections <- function(searchConditionsList = NULL, TempStudentGradingScaleGroupStudentSectionID = F, StudentID = F, StudentSectionID = F, SectionLengthID = F, SectionID = F, StudentFullNameLFM = F, CourseDescriptionCodeSectionCode = F, SectionLengthCode = F, HasError = F, ErrorText = F, GradingPeriodGradeBuckets = F, StudentGradingScaleGroupStudentSectionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsSelfPacedSection = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempStudentGradingScaleGroupStudentSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGradebookConfigDistrictYears <- function(searchConditionsList = NULL, ConfigDistrictYearID = F, DistrictID = F, SchoolYearID = F, UseCurriculumSubjectsInGradebook = F, ConfigDistrictYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ConfigDistrictYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listUnenteredStudentGradeBuckets <- function(searchConditionsList = NULL, StudentGradeBucketID = F, StudentSectionID = F, GradeMarkIDToApply = F, StudentID = F, StaffID = F, EntityID = F, SchoolYearID = F, SectionID = F, GradeBucketID = F, DisplayPeriodID = F, UseServiceIDOverride = F, GradeBucketLabel = F, GradeMarkCode = F, EntityCode = F, CourseDescription = F, Percent = F, StudentGradeBucketFlag = F, IsAdminOverride = F, StudentSectionTransactionStartDate = F, StudentSectionTransactionEndDate = F, GradingPeriodStartDate = F, GradingPeriodEndDate = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "UnenteredStudentGradeBucket", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStaffMeetLastScoreds <- function(searchConditionsList = NULL, StaffMeetLastScoredID = F, StaffMeetID = F, StudentAssignmentIDLastScored = F, TimeLastScored = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StaffMeetLastScored", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listContingentStudentAssignments <- function(searchConditionsList = NULL, StudentSectionID = F, AssignmentID = F, StudentAssignmentID = F, SectionID = F, EntityID = F, SchoolYearID = F, DueDate = F, IsEffectivelyMissing = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "ContingentStudentAssignment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStudentAssignmentScores <- function(searchConditionsList = NULL, TempStudentAssignmentScoreID = F, StudentAssignmentID = F, SectionID = F, OriginalScore = F, NewScore = F, OriginalGradeMarkID = F, NewGradeMarkID = F, OriginalGradeMarkCode = F, NewGradeMarkCode = F, MaxScore = F, Weight = F, AssignmentName = F, DueDate = F, StudentName = F, ClassName = F, ErrorMessage = F, AttemptCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "TempStudentAssignmentScore", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentGradeBucketTransferRuns <- function(searchConditionsList = NULL, StudentGradeBucketTransferRunID = F, StudentGradeBucketID = F, RunTime = F, Percent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Gradebook", objectName = "StudentGradeBucketTransferRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}
