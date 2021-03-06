% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{createVCRApplicationV1}
\alias{createVCRApplicationV1}
\title{Create a VCRApplicationV1}
\usage{
createVCRApplicationV1(
  ApplicationID = NULL,
  DistrictID = NULL,
  IsCategoricalFree = NULL,
  IsCategoricalNoChange = NULL,
  IsCategoricalNoResponse = NULL,
  IsCategoricalPaid = NULL,
  IsCategoricalReduced = NULL,
  IsCauseVerified = NULL,
  IsDirectVerification = NULL,
  IsErrorProne = NULL,
  IsFree = NULL,
  IsIncomeNoChange = NULL,
  IsIncomeNoResponse = NULL,
  IsIncomePaid = NULL,
  IsIncomeReduced = NULL,
  IsReduced = NULL,
  IsReducedFree = NULL,
  IsReducedNoChange = NULL,
  IsReducedNoResponse = NULL,
  IsReducedPaid = NULL,
  IsSelected = NULL,
  NameIDGuardian = NULL,
  NameIDPayor = NULL,
  SchoolYearID = NULL,
  VCRSubmissionID = NULL,
  VCRSubmissionRunHistoryID = NULL,
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

\item{fieldNames}{The field values to give the created VCRApplicationV1. Each defaults to NULL.}
}
\value{
A newly created VCRApplicationV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a VCRApplicationV1
}
\concept{StateReportingFederal}
