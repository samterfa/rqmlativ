% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{getVCRStudentV1}
\alias{getVCRStudentV1}
\title{Get a VCRStudentV1}
\usage{
getVCRStudentV1(
  VCRStudentV1ID,
  AddressID = F,
  CreatedTime = F,
  CustomerCategoryID = F,
  DistrictID = F,
  HasErrors = F,
  IsCategoricalFree = F,
  IsCategoricalNoChange = F,
  IsCategoricalNoResponse = F,
  IsCategoricalPaid = F,
  IsCategoricalReduced = F,
  IsCertifiedFreeMedicaid = F,
  IsCertifiedReducedMedicaid = F,
  IsDirectVerification = F,
  IsFree = F,
  IsIncomeNoChange = F,
  IsIncomeNoResponse = F,
  IsIncomePaid = F,
  IsIncomeReduced = F,
  IsOtherCertified = F,
  IsReduced = F,
  IsReducedFree = F,
  IsReducedNoChange = F,
  IsReducedNoResponse = F,
  IsReducedPaid = F,
  IsSNAPCertified = F,
  IsSNAPLetterCertified = F,
  KeyHash = F,
  ModifiedTime = F,
  NameIDGuardian = F,
  NameIDStudent = F,
  SchoolYearID = F,
  Status = F,
  UpdateHash = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  VCRSubmissionID = F,
  VCRSubmissionRunHistoryID = F,
  WithdrawalDate = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{VCRStudentV1ID}{The ID of the VCRStudentV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('VCRStudentV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given VCRStudentV1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the VCRStudentV1.}
}
\value{
A dataframe or of VCRStudentV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a VCRStudentV1
}
\concept{StateReportingFederal}
