% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{getACAEmployerV2}
\alias{getACAEmployerV2}
\title{Get an ACAEmployerV2}
\usage{
getACAEmployerV2(
  ACAEmployerV2ID,
  ACASubmissionRunHistoryID = F,
  ACATransmissionID = F,
  ACATransmissionIDReceiptProvider = F,
  AcceptedByIRS = F,
  AddressLine1ElectronicFile = F,
  AddressLine1Printed = F,
  AddressLine2ElectronicFile = F,
  AddressLine2Printed = F,
  AddressType = F,
  CityElectronicFile = F,
  CityPrinted = F,
  ContactPersonFirstName = F,
  ContactPersonLastName = F,
  ContactPersonMiddleName = F,
  ContactPersonSuffix = F,
  ContactPhoneNumber = F,
  CountryName = F,
  CreatedTime = F,
  DistrictID = F,
  EmployerID = F,
  EmployerIdentificationNumber = F,
  GovernmentEntityAddressLine1 = F,
  GovernmentEntityAddressLine2 = F,
  GovernmentEntityCity = F,
  GovernmentEntityContactPersonFirstName = F,
  GovernmentEntityContactPersonLastName = F,
  GovernmentEntityContactPersonMiddleName = F,
  GovernmentEntityContactPersonSuffix = F,
  GovernmentEntityContactPhoneNumber = F,
  GovernmentEntityEmployerIdentificationNumber = F,
  GovernmentEntityNameLine1 = F,
  GovernmentEntityNameLine2 = F,
  GovernmentEntityStateCode = F,
  GovernmentEntityZipCode = F,
  GovernmentEntityZipCodeExtension = F,
  HasErrors = F,
  HasSkywardFatalError = F,
  Is98PercentOfferMethod = F,
  IsAuthoritativeTransmittal = F,
  IsLatestInOriginalTransmissionSet = F,
  IsMainEmployer = F,
  IsMemberOfAggregatedALEGroup = F,
  IsQualifyingOfferMethod = F,
  KeyHash = F,
  ModifiedTime = F,
  NameLine1ElectronicFile = F,
  NameLine2ElectronicFile = F,
  NamePrinted = F,
  PostalCode = F,
  StateIDOrProvince = F,
  StateOrProvinceCode = F,
  StateOrProvinceName = F,
  TotalForms1095CFiled = F,
  TransmitterControlCode = F,
  UniqueExportIdentifierSelf = F,
  UpdateHash = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  W2CountryCode = F,
  W2CountryID = F,
  ZipCode = F,
  ZipCodeExtension = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{ACAEmployerV2ID}{The ID of the ACAEmployerV2 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ACAEmployerV2') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given ACAEmployerV2. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ACAEmployerV2.}
}
\value{
A dataframe or of ACAEmployerV2
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of an ACAEmployerV2
}
\concept{StateReportingFederal}
