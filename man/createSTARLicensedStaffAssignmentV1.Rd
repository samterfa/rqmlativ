% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{createSTARLicensedStaffAssignmentV1}
\alias{createSTARLicensedStaffAssignmentV1}
\title{Create a STARLicensedStaffAssignmentV1}
\usage{
createSTARLicensedStaffAssignmentV1(
  LengthOfPeriod = NULL,
  PeriodsPerWeek = NULL,
  SchoolNumber = NULL,
  STARLicensedStaffEmploymentV1ID = NULL,
  StateSTARAssignmentCodeMNID = NULL,
  StateSTARAssignmentCodeMNValue = NULL,
  StateSTARGradeLevelMNID = NULL,
  StateSTARGradeLevelMNValue = NULL,
  StateSTARModeOfTeachingMNID = NULL,
  StateSTARModeOfTeachingMNValue = NULL,
  TotalNumberOfPupils = NULL,
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

\item{fieldNames}{The field values to give the created STARLicensedStaffAssignmentV1. Each defaults to NULL.}
}
\value{
A newly created STARLicensedStaffAssignmentV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a STARLicensedStaffAssignmentV1
}
\concept{StateReportingMN}
