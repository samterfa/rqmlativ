% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Enrollment.R
\name{getStudentEntityYear}
\alias{getStudentEntityYear}
\title{Get a StudentEntityYear}
\usage{
getStudentEntityYear(
  StudentEntityYearID,
  ChromebookDocumentsReturned = F,
  CreatedTime = F,
  CurrentPercentEnrolled = F,
  DaysAbsentYTD = F,
  DaysEnrolledYTD = F,
  DaysExcusedYTD = F,
  DaysOtherYTD = F,
  DaysUnexcusedYTD = F,
  EntityID = F,
  EntryWithdrawalIDLatest = F,
  ExcludeFromHonorRoll = F,
  ExcludeFromRank = F,
  ExistsConflictedStudentCourseRequests = F,
  ExistsUnscheduleableStudentSections = F,
  FeeAmountDue = F,
  FeeChargeAmount = F,
  FeePaidAmount = F,
  FeePaidAndWaivedAmount = F,
  FeeUnappliedAmount = F,
  FeeWaivedAmount = F,
  FirstName = F,
  FlaggedMissingAssignmentsCount = F,
  Grade = F,
  HandbookSigned = F,
  HasActiveCareerPlanDeclarationTimePeriod = F,
  HasActiveEndorsementDeclarationTimePeriod = F,
  HasConflictedStudentCourseRequest = F,
  HasFlaggedMissingAssignments = F,
  HasMissingAssignments = F,
  HasNoAttendanceToday = F,
  HasNonCrossEntityCourseSchedulingEntryWithdrawal = F,
  HasOpenDisplayPeriodsInRegularSchoolDay = F,
  HasOverscheduledPeriod = F,
  HasValidStudentPlan = F,
  HomeroomCodeFollettDestiny = F,
  HomeroomID = F,
  HomeroomPeriodFollettDestiny = F,
  HomeroomStaffNameFollettDestiny = F,
  IncludeAsProspectiveRank = F,
  IsActive = F,
  IsCrossEntityCourseEnrollment = F,
  IsDefaultEntity = F,
  IsTransportationRequested = F,
  LastName = F,
  MiddleName = F,
  ModifiedTime = F,
  NameID = F,
  NumberOfStudentCourseRequests = F,
  NumberOfStudentSections = F,
  OptOutOfMedia = F,
  SchedulingCategories = F,
  SchedulingTeamID = F,
  SchoolIDPathExpectedSchool = F,
  SchoolYearID = F,
  SectionLengthAbsent = F,
  SectionLengthEnrolled = F,
  Semester2Absent = F,
  Semester2Enrolled = F,
  SignedAcceptableUsePolicy = F,
  StaffIDAdvisor = F,
  StaffIDDisciplineOfficer = F,
  StudentID = F,
  StudentNumber = F,
  TardyCountYTD = F,
  TardyKioskTotals = F,
 
    TotalEarnedCreditsPossibleAnticipatedNonTransferStudentSectionsNonAlternateRequestCredits = F,
  TotalMissedAssignmentCount = F,
  UILFeeReceived = F,
  UnscheduleableStudentSectionCount = F,
  UnscheduledStudentCourseRequestCount = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  WithdrawalDate = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{StudentEntityYearID}{The ID of the StudentEntityYear to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEntityYear') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given StudentEntityYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEntityYear.}
}
\value{
A dataframe or of StudentEntityYear
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a StudentEntityYear
}
\concept{Enrollment}
