% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReporting.R
\name{modifyUnemploymentCompensationEmployeeV1}
\alias{modifyUnemploymentCompensationEmployeeV1}
\title{Modify an UnemploymentCompensationEmployeeV1}
\usage{
modifyUnemploymentCompensationEmployeeV1(
  UnemploymentCompensationEmployeeV1ID,
  AccumulatedDeductionsReducingTaxableWagesForPriorQuarters = NULL,
  AccumulatedGrossWagesForPriorQuarters = NULL,
  BuildingID = NULL,
  EmployeeID = NULL,
  FirstName = NULL,
  GrossWagesMonth1 = NULL,
  GrossWagesMonth2 = NULL,
  GrossWagesMonth3 = NULL,
  LastName = NULL,
  MiddleName = NULL,
  PaidFor12thDayOfMonth1 = NULL,
  PaidFor12thDayOfMonth2 = NULL,
  PaidFor12thDayOfMonth3 = NULL,
  QuarterlyDeductionsReducingTaxableWages = NULL,
  SocialSecurityNumber = NULL,
  UnemploymentCompensationSubmissionID = NULL,
  UnemploymentCompensationSubmissionRunHistoryID = NULL,
  Worksite = NULL,
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

\item{fieldNames}{The field values to give the modified UnemploymentCompensationEmployeeV1. Each defaults to NULL.}
}
\value{
The modified UnemploymentCompensationEmployeeV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies an UnemploymentCompensationEmployeeV1
}
\concept{StateReporting}
