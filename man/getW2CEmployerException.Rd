% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{getW2CEmployerException}
\alias{getW2CEmployerException}
\title{Get a W2CEmployerException}
\usage{
getW2CEmployerException(
  W2CEmployerExceptionID,
  CreatedTime = F,
  Message = F,
  MessageType = F,
  ModifiedTime = F,
  ProcessType = F,
  RuleNumber = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  W2CEmployerV1ID = F,
  W2CSubmissionID = F,
  W2CSubmissionRunHistoryID = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{W2CEmployerExceptionID}{The ID of the W2CEmployerException to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('W2CEmployerException') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given W2CEmployerException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the W2CEmployerException.}
}
\value{
A dataframe or of W2CEmployerException
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a W2CEmployerException
}
\concept{StateReportingFederal}
