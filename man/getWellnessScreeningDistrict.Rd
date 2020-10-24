% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Health.R
\name{getWellnessScreeningDistrict}
\alias{getWellnessScreeningDistrict}
\title{Get a WellnessScreeningDistrict}
\usage{
getWellnessScreeningDistrict(
  WellnessScreeningDistrictID,
  WellnessScreeningID = F,
  DistrictID = F,
  DistrictGroupKey = F,
  DisplayName = F,
  Enable = F,
  ScreeningFrequency = F,
  QuickEntryFailedHeading = F,
  QuickEntryFailedInstructions = F,
  StaffFailedHeading = F,
  StaffFailedInstructions = F,
  StaffPassedHeading = F,
  StaffPassedInstructions = F,
  StudentFailedHeading = F,
  StudentFailedInstructions = F,
  StudentPassedHeading = F,
  StudentPassedInstructions = F,
  EmployeeFailedHeading = F,
  EmployeeFailedInstructions = F,
  EmployeePassedHeading = F,
  EmployeePassedInstructions = F,
  UserIDCreatedBy = F,
  CreatedTime = F,
  UserIDModifiedBy = F,
  ModifiedTime = F,
  CollectBodyTemperatureAdministrativeAccess = F,
  CollectBodyTemperatureEmployeeAccess = F,
  CollectBodyTemperatureFamilyAccess = F,
  CollectBodyTemperatureStudentAccess = F,
  CollectBodyTemperatureTeacherAccess = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{WellnessScreeningDistrictID}{The ID of the WellnessScreeningDistrict to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WellnessScreeningDistrict') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given WellnessScreeningDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WellnessScreeningDistrict.}
}
\value{
A dataframe or of WellnessScreeningDistrict
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a WellnessScreeningDistrict
}
\concept{Health}