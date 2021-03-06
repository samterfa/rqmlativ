% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReporting.R
\name{getBehaviorDescriptorDashboardV1}
\alias{getBehaviorDescriptorDashboardV1}
\title{Get a BehaviorDescriptorDashboardV1}
\usage{
getBehaviorDescriptorDashboardV1(
  BehaviorDescriptorDashboardV1ID,
  CategoryCode = F,
  Code = F,
  CreatedTime = F,
  DashboardRunHistoryID = F,
  Description = F,
  EdFiBehaviorCategoryTypeID = F,
  EntityID = F,
  KeyHash = F,
  ModifiedTime = F,
  SchoolCode = F,
  SchoolYearID = F,
  ShortDescription = F,
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
\item{BehaviorDescriptorDashboardV1ID}{The ID of the BehaviorDescriptorDashboardV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BehaviorDescriptorDashboardV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given BehaviorDescriptorDashboardV1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BehaviorDescriptorDashboardV1.}
}
\value{
A dataframe or of BehaviorDescriptorDashboardV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a BehaviorDescriptorDashboardV1
}
\concept{StateReporting}
