% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{getDIRSVictimV1Exception}
\alias{getDIRSVictimV1Exception}
\title{Get a DIRSVictimV1Exception}
\usage{
getDIRSVictimV1Exception(
  DIRSVictimV1ExceptionID,
  CreatedTime = F,
  DIRSSubmissionID = F,
  DIRSSubmissionRunHistoryID = F,
  DIRSVictimV1ID = F,
  EntityID = F,
  IncidentOffenseNameID = F,
  LinkedToStaff = F,
  LinkedToStudent = F,
  Message = F,
  MessageType = F,
  ModifiedTime = F,
  ProcessType = F,
  RuleNumber = F,
  SchoolYearID = F,
  StaffID = F,
  StudentID = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  VictimName = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{DIRSVictimV1ExceptionID}{The ID of the DIRSVictimV1Exception to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DIRSVictimV1Exception') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given DIRSVictimV1Exception. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DIRSVictimV1Exception.}
}
\value{
A dataframe or of DIRSVictimV1Exception
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a DIRSVictimV1Exception
}
\concept{StateReportingMN}
