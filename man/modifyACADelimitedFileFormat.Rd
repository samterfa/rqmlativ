% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{modifyACADelimitedFileFormat}
\alias{modifyACADelimitedFileFormat}
\title{Modify an ACADelimitedFileFormat}
\usage{
modifyACADelimitedFileFormat(
  ACADelimitedFileFormatID,
  ACAThirdPartyFormatID = NULL,
  BirthDateColumnNumber = NULL,
  CommentColumnNumber = NULL,
  DelimiterCharacter = NULL,
  DelimiterType = NULL,
  EmployeeIdentifierColumnNumber = NULL,
  EmployeeRequiredContributionColumnNumber = NULL,
  EmployerOfferedSelfInsuredCoverageColumnNumber = NULL,
  FirstNameColumnNumber = NULL,
  LastNameColumnNumber = NULL,
  MiddleNameColumnNumber = NULL,
  NumberOfHeaderRows = NULL,
  OfferOfCoverageCodeColumnNumber = NULL,
  PlanStartMonthOverrideColumnNumber = NULL,
  ReportEmployeeAsCoveredIndividualColumnNumber = NULL,
  SafeHarborColumnNumber = NULL,
  SSNColumnNumber = NULL,
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

\item{fieldNames}{The field values to give the modified ACADelimitedFileFormat. Each defaults to NULL.}
}
\value{
The modified ACADelimitedFileFormat
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies an ACADelimitedFileFormat
}
\concept{StateReportingFederal}
