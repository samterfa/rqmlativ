% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Health.R
\name{getPhysicalScreening}
\alias{getPhysicalScreening}
\title{Get a PhysicalScreening}
\usage{
getPhysicalScreening(
  PhysicalScreeningID,
  AtRiskForDiabetes = F,
  AttachmentCount = F,
  BodyMassIndex = F,
  BodyMassIndexPercentile = F,
  CreatedTime = F,
  DiabetesScreened = F,
  DisplayHeight = F,
  DistrictID = F,
  EntityIDEnrolledIn = F,
  FirstBloodPressureReading = F,
  HealthProfessionalIDExaminedBy = F,
  Height = F,
  HeightFeet = F,
  HeightInches = F,
  IsVoid = F,
  ModifiedTime = F,
  NameID = F,
  NameOfficeVisitID = F,
  SchoolYearID = F,
  ScreeningDate = F,
  SecondBloodPressureReading = F,
  SportsPhysical = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  UserIDVoidedBy = F,
  VoidedTime = F,
  VoidNote = F,
  Weight = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{PhysicalScreeningID}{The ID of the PhysicalScreening to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PhysicalScreening') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given PhysicalScreening. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PhysicalScreening.}
}
\value{
A dataframe or of PhysicalScreening
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a PhysicalScreening
}
\concept{Health}
