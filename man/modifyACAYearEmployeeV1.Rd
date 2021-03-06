% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{modifyACAYearEmployeeV1}
\alias{modifyACAYearEmployeeV1}
\title{Modify an ACAYearEmployeeV1}
\usage{
modifyACAYearEmployeeV1(
  ACAYearEmployeeV1ID,
  ACAYearID = NULL,
  AddressLine1ElectronicFile = NULL,
  AddressLine1Printed = NULL,
  AddressLine2ElectronicFile = NULL,
  AddressLine2Printed = NULL,
  CityElectronicFile = NULL,
  CityPrinted = NULL,
  ContactPhoneNumber = NULL,
  EmployeeID = NULL,
  EmployeeShareOfLowestCostMonthlyPremium = NULL,
  EmployerOfferedSelfInsuredCoverage = NULL,
  FederalACAOfferAndCoverageCode = NULL,
  FederalACAOfferAndCoverageID = NULL,
  FederalACASafeHarborCode = NULL,
  FederalACASafeHarborID = NULL,
  FirstNameElectronicFile = NULL,
  LastNameElectronicFile = NULL,
  MediaIDFullSSN = NULL,
  MediaIDMaskedSSN = NULL,
  MiddleNameElectronicFile = NULL,
  NamePrinted = NULL,
  PlanStartMonth = NULL,
  SocialSecurityNumber = NULL,
  StateCode = NULL,
  StateID = NULL,
  SuffixElectronicFile = NULL,
  TestScenarioIdentification = NULL,
  XMLData = NULL,
  ZipCode = NULL,
  ZipCodeExtension = NULL,
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

\item{fieldNames}{The field values to give the modified ACAYearEmployeeV1. Each defaults to NULL.}
}
\value{
The modified ACAYearEmployeeV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies an ACAYearEmployeeV1
}
\concept{StateReportingFederal}
