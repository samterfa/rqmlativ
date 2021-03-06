% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{createDIRSWeaponV1}
\alias{createDIRSWeaponV1}
\title{Create a DIRSWeaponV1}
\usage{
createDIRSWeaponV1(
  DIRSSubmissionID = NULL,
  DIRSSubmissionRunHistoryID = NULL,
  EntityID = NULL,
  ExternalIncidentID = NULL,
  IncidentID = NULL,
  IncidentOffenseNameID = NULL,
  IncidentOffenseNameWeaponID = NULL,
  IsGunCased = NULL,
  IsGunInTrunk = NULL,
  IsGunLoaded = NULL,
  MARSSNumber = NULL,
  OffenderActivityCode = NULL,
  OffenderActivityTypeCode = NULL,
  SchoolYearID = NULL,
  StateOffenderActivityMNID = NULL,
  StateWeaponTypeMNID = NULL,
  StudentID = NULL,
  WasUsedAsADangerousWeaponFederal = NULL,
  WasUsedAsADangerousWeaponState = NULL,
  WeaponType = NULL,
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

\item{fieldNames}{The field values to give the created DIRSWeaponV1. Each defaults to NULL.}
}
\value{
A newly created DIRSWeaponV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a DIRSWeaponV1
}
\concept{StateReportingMN}
