% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{getMARSSSubmissionRunHistory}
\alias{getMARSSSubmissionRunHistory}
\title{Get a MARSSSubmissionRunHistory}
\usage{
getMARSSSubmissionRunHistory(
  MARSSSubmissionRunHistoryID,
  MARSSSubmissionID = F,
  StartDateTime = F,
  EndDateTime = F,
  Type = F,
  MediaID = F,
  IsExport = F,
  IsLocked = F,
  Status = F,
  RunData = F,
  UserIDCanceledBy = F,
  UserIDCreatedBy = F,
  CreatedTime = F,
  UserIDModifiedBy = F,
  ModifiedTime = F,
  ScopeAccessAllowed = F,
  HasValidMedia = F,
  CanAccessMedia = F,
  RunParameters = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{MARSSSubmissionRunHistoryID}{The ID of the MARSSSubmissionRunHistory to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MARSSSubmissionRunHistory') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given MARSSSubmissionRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MARSSSubmissionRunHistory.}
}
\value{
A dataframe or of MARSSSubmissionRunHistory
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a MARSSSubmissionRunHistory
}
\concept{StateReportingMN}