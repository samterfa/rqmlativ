% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{modifyForm941V1}
\alias{modifyForm941V1}
\title{Modify a Form941V1}
\usage{
modifyForm941V1(
  Form941V1ID,
  AdditionalTaxableMedicareWages = NULL,
  EmployeesPaidOn12th = NULL,
  EmployerCity = NULL,
  EmployerIdentificationNumber = NULL,
  EmployerName = NULL,
  EmployerPrimaryAddressLine = NULL,
  EmployerState = NULL,
  EmployerZip = NULL,
  FederalTaxWithheld = NULL,
  Form941ExtractRunID = NULL,
  MedicareTaxLiability = NULL,
  Quarter = NULL,
  SocialSecurityTaxLiability = NULL,
  Status = NULL,
  TaxableFederalWages = NULL,
  TaxableMedicareWages = NULL,
  TaxableSocialSecurityWages = NULL,
  TaxLiabilityMonth1 = NULL,
  TaxLiabilityMonth2 = NULL,
  TaxLiabilityMonth3 = NULL,
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

\item{fieldNames}{The field values to give the modified Form941V1. Each defaults to NULL.}
}
\value{
The modified Form941V1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a Form941V1
}
\concept{StateReportingFederal}
