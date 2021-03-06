% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Workflow.R
\name{getTaskInstanceUser}
\alias{getTaskInstanceUser}
\title{Get a TaskInstanceUser}
\usage{
getTaskInstanceUser(
  TaskInstanceUserID,
  CreatedTime = F,
  Description = F,
  ModifiedTime = F,
  TaskInstanceID = F,
  Type = F,
  TypeCode = F,
  UserID = F,
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
\item{TaskInstanceUserID}{The ID of the TaskInstanceUser to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TaskInstanceUser') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given TaskInstanceUser. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TaskInstanceUser.}
}
\value{
A dataframe or of TaskInstanceUser
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a TaskInstanceUser
}
\concept{Workflow}
