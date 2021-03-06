% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{getCarlPerkinsSubmissionFileV1Exception}
\alias{getCarlPerkinsSubmissionFileV1Exception}
\title{Get a CarlPerkinsSubmissionFileV1Exception}
\usage{
getCarlPerkinsSubmissionFileV1Exception(
  CarlPerkinsSubmissionFileV1ExceptionID,
  CarlPerkinsSubmissionFileV1ID = F,
  CarlPerkinsSubmissionID = F,
  CarlPerkinsSubmissionRunHistoryID = F,
  CourseID = F,
  CreatedTime = F,
  EntityID = F,
  Message = F,
  MessageType = F,
  ModifiedTime = F,
  ProcessType = F,
  RuleNumber = F,
  SchoolID = F,
  SchoolYearID = F,
  StudentGradeBucketID = F,
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
\item{CarlPerkinsSubmissionFileV1ExceptionID}{The ID of the CarlPerkinsSubmissionFileV1Exception to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CarlPerkinsSubmissionFileV1Exception') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given CarlPerkinsSubmissionFileV1Exception. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CarlPerkinsSubmissionFileV1Exception.}
}
\value{
A dataframe or of CarlPerkinsSubmissionFileV1Exception
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a CarlPerkinsSubmissionFileV1Exception
}
\concept{StateReportingMN}
