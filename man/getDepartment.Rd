% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Staff.R
\name{getDepartment}
\alias{getDepartment}
\title{Get a Department}
\usage{
getDepartment(
  DepartmentID,
  SchoolYearID = F,
  EntityID = F,
  EntityGroupKey = F,
  Code = F,
  Description = F,
  StaffIDDepartmentHead = F,
  DepartmentIDClonedFrom = F,
  CodeDescription = F,
  DepartmentIDClonedTo = F,
  UserIDCreatedBy = F,
  CreatedTime = F,
  UserIDModifiedBy = F,
  ModifiedTime = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{DepartmentID}{The ID of the Department to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Department') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given Department. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Department.}
}
\value{
A dataframe or of Department
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a Department
}
\concept{Staff}