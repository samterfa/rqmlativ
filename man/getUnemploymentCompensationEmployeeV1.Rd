% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReporting.R
\name{getUnemploymentCompensationEmployeeV1}
\alias{getUnemploymentCompensationEmployeeV1}
\title{Get an UnemploymentCompensationEmployeeV1}
\usage{
getUnemploymentCompensationEmployeeV1(
  UnemploymentCompensationEmployeeV1ID,
  AccumulatedDeductionsReducingTaxableWagesForPriorQuarters = F,
  AccumulatedGrossWagesForCurrentYear = F,
  AccumulatedGrossWagesForPriorQuarters = F,
  AccumulatedTaxableWagesForPriorQuarters = F,
  BuildingID = F,
  CreatedTime = F,
  EmployeeID = F,
  FirstName = F,
  GrossWagesMonth1 = F,
  GrossWagesMonth2 = F,
  GrossWagesMonth3 = F,
  HasErrors = F,
  KeyHash = F,
  LastName = F,
  MiddleName = F,
  ModifiedTime = F,
  PaidFor12thDayOfMonth1 = F,
  PaidFor12thDayOfMonth2 = F,
  PaidFor12thDayOfMonth3 = F,
  QuarterlyDeductionsReducingTaxableWages = F,
  QuarterlyGrossWagesEarned = F,
  QuarterlyTaxableWagesEarned = F,
  SocialSecurityNumber = F,
  UnemploymentCompensationSubmissionID = F,
  UnemploymentCompensationSubmissionRunHistoryID = F,
  UpdateHash = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  Worksite = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{UnemploymentCompensationEmployeeV1ID}{The ID of the UnemploymentCompensationEmployeeV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UnemploymentCompensationEmployeeV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given UnemploymentCompensationEmployeeV1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UnemploymentCompensationEmployeeV1.}
}
\value{
A dataframe or of UnemploymentCompensationEmployeeV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of an UnemploymentCompensationEmployeeV1
}
\concept{StateReporting}
