% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReporting.R
\name{getLocationDashboardV1}
\alias{getLocationDashboardV1}
\title{Get a LocationDashboardV1}
\usage{
getLocationDashboardV1(
  LocationDashboardV1ID,
  ClassroomIdentificationCode = F,
  CreatedTime = F,
  DashboardRunHistoryID = F,
  DistrictID = F,
  KeyHash = F,
  MaximumNumberOfSeats = F,
  ModifiedTime = F,
  OptimalNumberOfSeats = F,
  SchoolID = F,
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
\item{LocationDashboardV1ID}{The ID of the LocationDashboardV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LocationDashboardV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given LocationDashboardV1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LocationDashboardV1.}
}
\value{
A dataframe or of LocationDashboardV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a LocationDashboardV1
}
\concept{StateReporting}
