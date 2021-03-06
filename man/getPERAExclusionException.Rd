% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{getPERAExclusionException}
\alias{getPERAExclusionException}
\title{Get a PERAExclusionException}
\usage{
getPERAExclusionException(
  PERAExclusionExceptionID,
  CreatedTime = F,
  IsFatal = F,
  Message = F,
  ModifiedTime = F,
  PERAExclusionEmployeeV1ID = F,
  PERAExclusionExtractRunID = F,
  PreserveException = F,
  Source = F,
  Type = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{PERAExclusionExceptionID}{The ID of the PERAExclusionException to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PERAExclusionException') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given PERAExclusionException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PERAExclusionException.}
}
\value{
A dataframe or of PERAExclusionException
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a PERAExclusionException
}
\concept{StateReportingMN}
