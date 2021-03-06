% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{modifyW2CEmployeeV1}
\alias{modifyW2CEmployeeV1}
\title{Modify a W2CEmployeeV1}
\usage{
modifyW2CEmployeeV1(
  W2CEmployeeV1ID,
  Box12aCode = NULL,
  Box12bCode = NULL,
  Box12cCode = NULL,
  Box12dCode = NULL,
  City = NULL,
  CorrectBox10DependentCareBenefits = NULL,
  CorrectBox11NonQualifiedPlansNonSection457b = NULL,
  CorrectBox11NonQualifiedPlansSection457b = NULL,
  CorrectBox12aValue = NULL,
  CorrectBox12bValue = NULL,
  CorrectBox12cValue = NULL,
  CorrectBox12dValue = NULL,
  CorrectBox13IsRetirementPlan = NULL,
  CorrectBox13IsStatutoryEmployee = NULL,
  CorrectBox13IsThirdPartySickPay = NULL,
  CorrectBox1FederalWages = NULL,
  CorrectBox2FederalIncomeTaxWithheld = NULL,
  CorrectBox3SocialSecurityWages = NULL,
  CorrectBox4SocialSecurityTaxWithheld = NULL,
  CorrectBox5MedicareWages = NULL,
  CorrectBox6MedicareTaxWithheld = NULL,
  CorrectBox7SocialSecurityTips = NULL,
  CorrectFirstName = NULL,
  CorrectLastName = NULL,
  CorrectMiddleName = NULL,
  CorrectSSN = NULL,
  CountryCode = NULL,
  DeliveryAddress = NULL,
  EmployeeAddressType = NULL,
  EmployeeID = NULL,
  FederalW2Box12aCodeID = NULL,
  FederalW2Box12bCodeID = NULL,
  FederalW2Box12cCodeID = NULL,
  FederalW2Box12dCodeID = NULL,
  ForeignPostalCode = NULL,
  ForeignStateProvince = NULL,
  LocationAddress = NULL,
  MediaIDIndividualPDF = NULL,
  OriginallyReportedBox10DependentCareBenefits = NULL,
  OriginallyReportedBox11NonQualifiedPlansNonSection457b = NULL,
  OriginallyReportedBox11NonQualifiedPlansSection457b = NULL,
  OriginallyReportedBox12aValue = NULL,
  OriginallyReportedBox12bValue = NULL,
  OriginallyReportedBox12cValue = NULL,
  OriginallyReportedBox12dValue = NULL,
  OriginallyReportedBox13IsRetirementPlan = NULL,
  OriginallyReportedBox13IsStatutoryEmployee = NULL,
  OriginallyReportedBox13IsThirdPartySickPay = NULL,
  OriginallyReportedBox1FederalWages = NULL,
  OriginallyReportedBox2FederalIncomeTaxWithheld = NULL,
  OriginallyReportedBox3SocialSecurityWages = NULL,
  OriginallyReportedBox4SocialSecurityTaxWithheld = NULL,
  OriginallyReportedBox5MedicareWages = NULL,
  OriginallyReportedBox6MedicareTaxWithheld = NULL,
  OriginallyReportedBox7SocialSecurityTips = NULL,
  OriginallyReportedFirstName = NULL,
  OriginallyReportedLastName = NULL,
  OriginallyReportedMiddleName = NULL,
  OriginallyReportedSSN = NULL,
  StateCode = NULL,
  StateID = NULL,
  W2CountryID = NULL,
  W2CSubmissionID = NULL,
  W2CSubmissionRunHistoryID = NULL,
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

\item{fieldNames}{The field values to give the modified W2CEmployeeV1. Each defaults to NULL.}
}
\value{
The modified W2CEmployeeV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a W2CEmployeeV1
}
\concept{StateReportingFederal}
