% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{deleteW2CEmployeeV1}
\alias{deleteW2CEmployeeV1}
\title{Delete a W2CEmployeeV1}
\usage{
deleteW2CEmployeeV1(
  W2CEmployeeV1ID,
  ignoreWarnings = F,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{W2CEmployeeV1ID}{The ID of the W2CEmployeeV1 to delete}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}
}
\value{
The W2CEmployeeV1ID of the deleted W2CEmployeeV1.
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function deletes a W2CEmployeeV1
}
\concept{StateReportingFederal}
