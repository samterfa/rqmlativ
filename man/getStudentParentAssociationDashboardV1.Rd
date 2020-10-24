% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReporting.R
\name{getStudentParentAssociationDashboardV1}
\alias{getStudentParentAssociationDashboardV1}
\title{Get a StudentParentAssociationDashboardV1}
\usage{
getStudentParentAssociationDashboardV1(
  StudentParentAssociationDashboardV1ID,
  DistrictID = F,
  SchoolYearID = F,
  StudentID = F,
  NameIDParent = F,
  EdFiRelationTypeID = F,
  RelationTypeCode = F,
  IsPrimaryContact = F,
  IsEmergencyContact = F,
  DashboardRunHistoryID = F,
  UserIDCreatedBy = F,
  CreatedTime = F,
  UserIDModifiedBy = F,
  ModifiedTime = F,
  KeyHash = F,
  UpdateHash = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{StudentParentAssociationDashboardV1ID}{The ID of the StudentParentAssociationDashboardV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentParentAssociationDashboardV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given StudentParentAssociationDashboardV1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentParentAssociationDashboardV1.}
}
\value{
A dataframe or of StudentParentAssociationDashboardV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a StudentParentAssociationDashboardV1
}
\concept{StateReporting}