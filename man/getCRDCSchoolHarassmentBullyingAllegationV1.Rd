% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{getCRDCSchoolHarassmentBullyingAllegationV1}
\alias{getCRDCSchoolHarassmentBullyingAllegationV1}
\title{Get a CRDCSchoolHarassmentBullyingAllegationV1}
\usage{
getCRDCSchoolHarassmentBullyingAllegationV1(
  CRDCSchoolHarassmentBullyingAllegationV1ID,
  CivilRightsDataCollectionID = F,
  CivilRightsDataCollectionRunHistoryID = F,
  CreatedTime = F,
  DisabilityBasedAllegationCount = F,
  FederalNCESSchoolID = F,
  ModifiedTime = F,
  OrientationBasedAllegationCount = F,
  RaceBasedAllegationCount = F,
  ReligionBasedAllegationCount = F,
  SexBasedAllegationCount = F,
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
\item{CRDCSchoolHarassmentBullyingAllegationV1ID}{The ID of the CRDCSchoolHarassmentBullyingAllegationV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CRDCSchoolHarassmentBullyingAllegationV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given CRDCSchoolHarassmentBullyingAllegationV1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CRDCSchoolHarassmentBullyingAllegationV1.}
}
\value{
A dataframe or of CRDCSchoolHarassmentBullyingAllegationV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a CRDCSchoolHarassmentBullyingAllegationV1
}
\concept{StateReportingFederal}
