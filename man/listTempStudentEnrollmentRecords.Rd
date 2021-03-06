% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Enrollment.R
\name{listTempStudentEnrollmentRecords}
\alias{listTempStudentEnrollmentRecords}
\title{List TempStudentEnrollmentRecords}
\usage{
listTempStudentEnrollmentRecords(
  searchConditionsList = NULL,
  AdvisorFullName = F,
  CalendarCode = F,
  CalendarID = F,
  CompletedSchoolYearOverride = F,
  CreatedTime = F,
  CreateFeeManagementCustomer = F,
  CreateFeeManagementCustomerEntityYear = F,
  DisciplineOfficerFullName = F,
  EdFiDistrictIDResidence = F,
  EdFiDistrictIDTransfer = F,
  EdFiDistrictResidenceCodeDescription = F,
  EdFiSchoolIDTransfer = F,
  EndDate = F,
  EnrollIntoEntityCode = F,
  EnrollmentMoveable = F,
  EntityCode = F,
  EntityID = F,
  EntryCode = F,
  EntryCodeID = F,
  EntryComment = F,
  EntryWithdrawalID = F,
  Error = F,
  ErrorCount = F,
  ExcludeFromHonorRoll = F,
  ExcludeFromRank = F,
  ExcludeFromThirdFridaySeptemberCount = F,
  FailureReason = F,
  FeeManagementCustomerID = F,
  GradeLevelCode = F,
  GradeReferenceID = F,
  GradYear = F,
  GSAADAClaimableOverrideCode = F,
  GSAADAClaimableOverrideCodeDisplayName = F,
  HomeRCDTSOverrideCode = F,
  HomeRCDTSOverrideCodeDisplayName = F,
  HomeRCDTSOverrideID = F,
  HomeroomCode = F,
  HomeroomID = F,
  IncludeAsProspectiveRank = F,
  IsCurrentActive = F,
  IsDefaultEntityForEntryWithdrawal = F,
  IsDefaultEntityForStudentEntityYear = F,
  IsPermanentExit = F,
  IsPrivateSchoolChoiceStudent = F,
  IsTuitionPaidOutOfDistrict = F,
  ModifiedTime = F,
  NumericYear = F,
  OutgoingStudent = F,
  PercentEnrolled = F,
  ProcessEntryWithdrawal = F,
  PromotionStatus = F,
  ScheduledSectionCount = F,
  SchoolCode = F,
  SchoolID = F,
  SchoolYearID = F,
  ServingRCDTSOverrideCode = F,
  ServingRCDTSOverrideCodeDisplayName = F,
  ServingRCDTSOverrideID = F,
  SourceEntryWithdrawalID = F,
  StaffIDAdvisor = F,
  StaffIDDisciplineOfficer = F,
  StartDate = F,
  StateAidCategoryMNID = F,
  StateDistrictMNCodeName = F,
  StateDistrictMNID = F,
  StateLastAttendanceLocationCodeMNID = F,
  StudentCourseRequestNotMoveableCount = F,
  StudentCourseRequestToDeleteCount = F,
  StudentFullName = F,
  StudentID = F,
  StudentNumber = F,
  StudentTypeCode = F,
  StudentTypeID = F,
  TempStudentEnrollmentRecordID = F,
  TestingSchoolCode = F,
  TestingSchoolCodeDisplayName = F,
  TestingSchoolRCDTSOverrideCode = F,
  TestingSchoolRCDTSOverrideCodeDisplayName = F,
  TestingSchoolRCDTSOverrideID = F,
  TotalStudentCourseRequestCount = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  WithdrawalCode = F,
  WithdrawalCodeID = F,
  WithdrawalComment = F,
  fieldPaths = NULL,
  searchConditionsGroupType = "And",
  searchSortFieldNamesList = NULL,
  searchSortFieldNamesDescendingList = NULL,
  entityId = 1,
  query = NULL,
  page = 1,
  pageSize = 1e+05,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{searchConditionsList}{A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentEnrollmentRecord') to get more field paths.}

\item{searchConditionsGroupType}{The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.}

\item{searchSortFieldNamesList}{The list of fields sort results by. Defaults to NULL (unsorted).}

\item{searchSortFieldNamesDescendingList}{A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{page}{Results are paginated. The page of results to return. Default is 1.}

\item{pageSize}{Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).}

\item{flatten}{Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given TempStudentEnrollmentRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentEnrollmentRecords.}
}
\value{
A list of TempStudentEnrollmentRecords
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of TempStudentEnrollmentRecords
}
\concept{Enrollment}
