% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReporting.R
\name{getProgramDashboardV1}
\alias{getProgramDashboardV1}
\title{Get a ProgramDashboardV1}
\usage{
getProgramDashboardV1(
  ProgramDashboardV1ID,
  CreatedTime = F,
  DashboardRunHistoryID = F,
  DistrictID = F,
  KeyHash = F,
  ModifiedTime = F,
  ProgramType = F,
  SchoolYearID = F,
  UpdateHash = F,
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
\item{ProgramDashboardV1ID}{The ID of the ProgramDashboardV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ProgramDashboardV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given ProgramDashboardV1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ProgramDashboardV1.}
}
\value{
A dataframe or of ProgramDashboardV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a ProgramDashboardV1
}
\concept{StateReporting}
