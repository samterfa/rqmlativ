% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{modifyW2YearEmployeeLocalityV1}
\alias{modifyW2YearEmployeeLocalityV1}
\title{Modify a W2YearEmployeeLocalityV1}
\usage{
modifyW2YearEmployeeLocalityV1(
  W2YearEmployeeLocalityV1ID,
  Box18LocalWages = NULL,
  Box19LocalIncomeTax = NULL,
  IsOriginalExtractedValue = NULL,
  W2YearEmployeeV1ID = NULL,
  W2YearLocalityID = NULL,
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

\item{fieldNames}{The field values to give the modified W2YearEmployeeLocalityV1. Each defaults to NULL.}
}
\value{
The modified W2YearEmployeeLocalityV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a W2YearEmployeeLocalityV1
}
\concept{StateReportingFederal}
