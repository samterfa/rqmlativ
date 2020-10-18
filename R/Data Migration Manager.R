

	listDataMigrationManagers <- function(searchConditionsList = NULL, DataMigrationManagerID = F, ProcessedTime = F, MigrationType = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Label = F, SecondaryType = F, MediaID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "DataMigrationManager", objectName = "DataMigrationManager", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDataMigrationManagerExceptions <- function(searchConditionsList = NULL, DataMigrationManagerExceptionID = F, DataMigrationManagerID = F, SourcePrimaryKey = F, Object = F, Identifier = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExceptionType = F, Category = F, Col1 = F, Col2 = F, Col3 = F, Col4 = F, Col5 = F, Col6 = F, Col7 = F, Col8 = F, Col9 = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "DataMigrationManager", objectName = "DataMigrationManagerException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}
