% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{getStateReportingMNNewHireEmployeeV1}
\alias{getStateReportingMNNewHireEmployeeV1}
\title{Get a StateReportingMNNewHireEmployeeV1}
\usage{
getStateReportingMNNewHireEmployeeV1(
  StateReportingMNNewHireEmployeeV1ID,
  AddressID = F,
  AddressLine1 = F,
  AddressLine2 = F,
  AddressLine3 = F,
  BirthDate = F,
  City = F,
  CountryCode = F,
  CreatedTime = F,
  EmployeeID = F,
  FirstNameLegal = F,
  HireDate = F,
  HireStateCode = F,
  IsIndependentContractor = F,
  IsRehire = F,
  LastNameLegal = F,
  MiddleNameLegal = F,
  ModifiedTime = F,
  NewHireEmployeeV1ID = F,
  NewHireEmployerV1ID = F,
  SocialSecurityNumber = F,
  StateCode = F,
  StateIDHire = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  ZipCode = F,
  ZipCodePlusFour = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{StateReportingMNNewHireEmployeeV1ID}{The ID of the StateReportingMNNewHireEmployeeV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateReportingMNNewHireEmployeeV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given StateReportingMNNewHireEmployeeV1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateReportingMNNewHireEmployeeV1.}
}
\value{
A dataframe or of StateReportingMNNewHireEmployeeV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a StateReportingMNNewHireEmployeeV1
}
\concept{StateReportingMN}
