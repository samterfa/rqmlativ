% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReporting.R
\name{modifyChildSupportException}
\alias{modifyChildSupportException}
\title{Modify a ChildSupportException}
\usage{
modifyChildSupportException(
  ChildSupportExceptionID,
  ChildSupportDetailV1ID = NULL,
  ChildSupportExtractRunID = NULL,
  EmployeeID = NULL,
  IsFatal = NULL,
  Message = NULL,
  PreserveException = NULL,
  Type = NULL,
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

\item{fieldNames}{The field values to give the modified ChildSupportException. Each defaults to NULL.}
}
\value{
The modified ChildSupportException
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a ChildSupportException
}
\concept{StateReporting}
