% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{deleteFederalEEO4Function}
\alias{deleteFederalEEO4Function}
\title{Delete a FederalEEO4Function}
\usage{
deleteFederalEEO4Function(
  FederalEEO4FunctionID,
  ignoreWarnings = F,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{FederalEEO4FunctionID}{The ID of the FederalEEO4Function to delete}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}
}
\value{
The FederalEEO4FunctionID of the deleted FederalEEO4Function.
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function deletes a FederalEEO4Function
}
\concept{StateReportingFederal}
