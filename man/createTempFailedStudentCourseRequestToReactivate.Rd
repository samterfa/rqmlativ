% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Scheduling.R
\name{createTempFailedStudentCourseRequestToReactivate}
\alias{createTempFailedStudentCourseRequestToReactivate}
\title{Create a TempFailedStudentCourseRequestToReactivate}
\usage{
createTempFailedStudentCourseRequestToReactivate(
  AlternateRank = NULL,
  CourseCode = NULL,
  CourseCodeDescription = NULL,
  CourseDescription = NULL,
  CourseEntityOfferedToID = NULL,
  CourseGradeLevelSummary = NULL,
  CourseID = NULL,
  CurrentEnrollment = NULL,
  DateFrom = NULL,
  DateTo = NULL,
  Days = NULL,
  EarlyExitReasonCodeDescription = NULL,
  EarlyExitReasonID = NULL,
  EarnedCreditOverride = NULL,
  EarnedCredits = NULL,
  EndDate = NULL,
  EntityIDCountsAs = NULL,
  EntityIDCourse = NULL,
  EntityIDRequestedFrom = NULL,
  ExcludeFromReportCardsAndTranscripts = NULL,
  ExcludeFromStudentSectionLink = NULL,
  GradeReferenceID = NULL,
  IsAlternate = NULL,
  IsTransferCourse = NULL,
  MaximumStudentCount = NULL,
  NameIDRequestedBy = NULL,
  NameRequestedByLFM = NULL,
  Note = NULL,
  Period = NULL,
  PreventReactivateCheckboxFromBeingRendered = NULL,
  RequestSource = NULL,
  RequestStatus = NULL,
  SchedulingMethod = NULL,
  SchoolYearIDCourse = NULL,
  SectionCode = NULL,
  SectionID = NULL,
  SectionLengthID = NULL,
  SectionLengthSubsetCode = NULL,
  SectionLengthSubsetDescription = NULL,
  SectionLengthSubsetID = NULL,
  StaffFullNameFML = NULL,
  StartDate = NULL,
  StudentCourseRequestID = NULL,
  StudentCourseRequestIDAlternateFor = NULL,
  StudentID = NULL,
  StudentSectionID = NULL,
  StudentSectionTransactionID = NULL,
  TempRecordToReactivatePrimaryKeyValue = NULL,
  TotalEarnedCreditOverride = NULL,
  TotalFailedCreditOverride = NULL,
  TransferCourseName = NULL,
  UseEarnedCreditOverride = NULL,
  UseEarnedCreditTotalOverride = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{The field values to give the created TempFailedStudentCourseRequestToReactivate. Each defaults to NULL.}
}
\value{
A newly created TempFailedStudentCourseRequestToReactivate
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a TempFailedStudentCourseRequestToReactivate
}
\concept{Scheduling}
