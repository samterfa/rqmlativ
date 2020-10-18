

	listOneRosterUsers <- function(searchConditionsList = NULL, UserID = F, StaffID = F, StudentID = F, Status = F, Username = F, EnabledUser = F, GivenName = F, FamilyName = F, MiddleName = F, Identifier = F, Email = F, PhoneNumber = F, Org = F, Grade = F, SourcedID = F, ModifiedTime = F, Role = F, NameID = F, UserIDs = F, OrgSourcedIDs = F, SMS = F, AgentSourcedIDs = F, Password = F, OneRosterID = F, DateLastModified = F, GuardianID = F, Agent = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "OneRoster", objectName = "User", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAcademicSessions <- function(searchConditionsList = NULL, AcademicSessionID = F, SectionLengthID = F, GradingPeriodID = F, Title = F, StartDate = F, EndDate = F, SchoolYear = F, Parent = F, Children = F, SourcedID = F, ModifiedTime = F, SchoolYearID = F, SessionType = F, Status = F, OneRosterID = F, DateLastModified = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "OneRoster", objectName = "AcademicSession", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEnrollments <- function(searchConditionsList = NULL, EnrollmentID = F, StudentSectionTransactionID = F, StaffMeetID = F, ClassSourcedID = F, SchoolSourcedID = F, UserSourcedID = F, IsPrimary = F, BeginDate = F, EndDate = F, SourcedID = F, ModifiedTime = F, Role = F, Status = F, OneRosterID = F, DateLastModified = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "OneRoster", objectName = "Enrollment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listOrgs <- function(searchConditionsList = NULL, OrgID = F, EntityID = F, DepartmentID = F, Name = F, Identifier = F, Parent = F, Children = F, SourcedID = F, ModifiedTime = F, SubType = F, Status = F, OneRosterID = F, DateLastModified = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "OneRoster", objectName = "Org", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listClasses <- function(searchConditionsList = NULL, ClassID = F, SectionID = F, Title = F, ClassCode = F, ClassType = F, Location = F, Grades = F, Subjects = F, Course = F, School = F, Terms = F, Periods = F, SubjectCodes = F, Resources = F, SourcedID = F, Status = F, ModifiedTime = F, CourseSourcedID = F, SchoolSourcedID = F, TermSourcedIds = F, OneRosterID = F, DateLastModified = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "OneRoster", objectName = "Class", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listOneRosterCourses <- function(searchConditionsList = NULL, CourseID = F, Title = F, SchoolYear = F, CourseCode = F, Grades = F, Subjects = F, Org = F, SubjectCodes = F, Resources = F, SourcedID = F, Status = F, ModifiedTime = F, SchoolYearSourcedID = F, OrgSourcedID = F, OneRosterID = F, DateLastModified = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "OneRoster", objectName = "Course", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDemographics <- function(searchConditionsList = NULL, DemographicID = F, StaffID = F, StudentID = F, BirthDate = F, Sex = F, AmericanIndianOrAlaskaNative = F, Asian = F, BlackOrAfricanAmerican = F, NativeHawaiianOrOtherPacificIslander = F, White = F, DemographicRaceTwoOrMoreRaces = F, HispanicOrLatinoEthnicity = F, CountryOfBirthCode = F, StateOfBirthAbbreviation = F, CityOfBirth = F, PublicSchoolResidenceStatus = F, SourcedID = F, Status = F, ModifiedTime = F, OneRosterID = F, DateLastModified = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "OneRoster", objectName = "Demographic", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listResults <- function(searchConditionsList = NULL, ResultID = F, Comment = F, Score = F, ScoreDate = F, ScoreStatus = F, LineItemSourcedID = F, ClassSourcedID = F, StudentSourcedID = F, AssignmentID = F, StudentAssignmentID = F, SourcedID = F, Status = F, OneRosterID = F, DateLastModified = F, ModifiedTime = F, VendorSourceID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "OneRoster", objectName = "Result", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listLineItems <- function(searchConditionsList = NULL, LineItemID = F, AssignmentID = F, Title = F, Description = F, AssignDate = F, DueDate = F, ClassSourcedID = F, CategorySourcedID = F, GradingPeriodSourcedID = F, ResultValueMin = F, ResultValueMax = F, SourcedID = F, Status = F, OneRosterID = F, DateLastModified = F, ModifiedTime = F, VendorSourceID = F, SectionID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "OneRoster", objectName = "LineItem", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listOneRosterCategories <- function(searchConditionsList = NULL, CategoryID = F, OneRosterCategoryID = F, Title = F, SourcedID = F, Status = F, OneRosterID = F, DateLastModified = F, ModifiedTime = F, DistrictID = F, SchoolYearID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "OneRoster", objectName = "Category", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}
