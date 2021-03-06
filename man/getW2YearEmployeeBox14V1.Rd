% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{getW2YearEmployeeBox14V1}
\alias{getW2YearEmployeeBox14V1}
\title{Get a W2YearEmployeeBox14V1}
\usage{
getW2YearEmployeeBox14V1(
  W2YearEmployeeBox14V1ID,
  Amount = F,
  Code = F,
  CreatedTime = F,
  Description = F,
  FederalW2BoxID = F,
  IsOriginalExtractedValue = F,
  ModifiedTime = F,
  OregonTransitTaxWages = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  W2YearBox14SetID = F,
  W2YearEmployeeV1ID = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{W2YearEmployeeBox14V1ID}{The ID of the W2YearEmployeeBox14V1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('W2YearEmployeeBox14V1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given W2YearEmployeeBox14V1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the W2YearEmployeeBox14V1.}
}
\value{
A dataframe or of W2YearEmployeeBox14V1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a W2YearEmployeeBox14V1
}
\concept{StateReportingFederal}
