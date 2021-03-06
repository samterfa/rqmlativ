% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{createStateReportingMNNewHireEmployerV1}
\alias{createStateReportingMNNewHireEmployerV1}
\title{Create a StateReportingMNNewHireEmployerV1}
\usage{
createStateReportingMNNewHireEmployerV1(
  AddressID = NULL,
  AddressIDSecondary = NULL,
  AddressLine1 = NULL,
  AddressLine2 = NULL,
  AddressLine3 = NULL,
  City = NULL,
  ContactPerson = NULL,
  ContactPhoneExtension = NULL,
  ContactPhoneNumber = NULL,
  CountryCode = NULL,
  EmployerID = NULL,
  EmployerName = NULL,
  FederalEIN = NULL,
  NewHireExtractRunID = NULL,
  SecondaryAddressLine1 = NULL,
  SecondaryAddressLine2 = NULL,
  SecondaryAddressLine3 = NULL,
  SecondaryCity = NULL,
  SecondaryContactPerson = NULL,
  SecondaryContactPhoneExtension = NULL,
  SecondaryContactPhoneNumber = NULL,
  SecondaryCountryCode = NULL,
  SecondaryStateCode = NULL,
  SecondaryZipCode = NULL,
  SecondaryZipCodePlusFour = NULL,
  StateCode = NULL,
  ZipCode = NULL,
  ZipCodePlusFour = NULL,
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

\item{fieldNames}{The field values to give the created StateReportingMNNewHireEmployerV1. Each defaults to NULL.}
}
\value{
A newly created StateReportingMNNewHireEmployerV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a StateReportingMNNewHireEmployerV1
}
\concept{StateReportingMN}
