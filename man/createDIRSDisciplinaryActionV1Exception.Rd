% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{createDIRSDisciplinaryActionV1Exception}
\alias{createDIRSDisciplinaryActionV1Exception}
\title{Create a DIRSDisciplinaryActionV1Exception}
\usage{
createDIRSDisciplinaryActionV1Exception(
  DIRSDisciplinaryActionV1ID = NULL,
  DIRSSubmissionID = NULL,
  DIRSSubmissionRunHistoryID = NULL,
  EntityID = NULL,
  IncidentOffenseNameActionID = NULL,
  Message = NULL,
  MessageType = NULL,
  ProcessType = NULL,
  RuleNumber = NULL,
  SchoolYearID = NULL,
  StudentID = NULL,
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

\item{fieldNames}{The field values to give the created DIRSDisciplinaryActionV1Exception. Each defaults to NULL.}
}
\value{
A newly created DIRSDisciplinaryActionV1Exception
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a DIRSDisciplinaryActionV1Exception
}
\concept{StateReportingMN}
