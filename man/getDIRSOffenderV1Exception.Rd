% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{getDIRSOffenderV1Exception}
\alias{getDIRSOffenderV1Exception}
\title{Get a DIRSOffenderV1Exception}
\usage{
getDIRSOffenderV1Exception(
  DIRSOffenderV1ExceptionID,
  CreatedTime = F,
  DIRSOffenderV1ID = F,
  DIRSSubmissionID = F,
  DIRSSubmissionRunHistoryID = F,
  EntityID = F,
  IncidentOffenseNameID = F,
  Message = F,
  MessageType = F,
  ModifiedTime = F,
  ProcessType = F,
  RuleNumber = F,
  SchoolYearID = F,
  StudentID = F,
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
\item{DIRSOffenderV1ExceptionID}{The ID of the DIRSOffenderV1Exception to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DIRSOffenderV1Exception') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given DIRSOffenderV1Exception. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DIRSOffenderV1Exception.}
}
\value{
A dataframe or of DIRSOffenderV1Exception
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a DIRSOffenderV1Exception
}
\concept{StateReportingMN}
