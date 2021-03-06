% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Enrollment.R
\name{createStudentEntityYear}
\alias{createStudentEntityYear}
\title{Create a StudentEntityYear}
\usage{
createStudentEntityYear(
  ChromebookDocumentsReturned = NULL,
  CurrentPercentEnrolled = NULL,
  EntityID = NULL,
  EntryWithdrawalIDLatest = NULL,
  ExcludeFromHonorRoll = NULL,
  ExcludeFromRank = NULL,
  Grade = NULL,
  HandbookSigned = NULL,
  HomeroomID = NULL,
  IncludeAsProspectiveRank = NULL,
  IsActive = NULL,
  IsCrossEntityCourseEnrollment = NULL,
  IsDefaultEntity = NULL,
  IsTransportationRequested = NULL,
  NameID = NULL,
  OptOutOfMedia = NULL,
  SchedulingTeamID = NULL,
  SchoolIDPathExpectedSchool = NULL,
  SchoolYearID = NULL,
  SignedAcceptableUsePolicy = NULL,
  StaffIDAdvisor = NULL,
  StaffIDDisciplineOfficer = NULL,
  StudentID = NULL,
  StudentNumber = NULL,
  UILFeeReceived = NULL,
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

\item{fieldNames}{The field values to give the created StudentEntityYear. Each defaults to NULL.}
}
\value{
A newly created StudentEntityYear
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a StudentEntityYear
}
\concept{Enrollment}
