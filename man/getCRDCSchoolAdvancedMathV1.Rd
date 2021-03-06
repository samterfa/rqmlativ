% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{getCRDCSchoolAdvancedMathV1}
\alias{getCRDCSchoolAdvancedMathV1}
\title{Get a CRDCSchoolAdvancedMathV1}
\usage{
getCRDCSchoolAdvancedMathV1(
  CRDCSchoolAdvancedMathV1ID,
  AdvancedCertifiedClassCount = F,
  AdvancedClassCount = F,
  AdvancedFemaleAlaskanCount = F,
  AdvancedFemaleAsianCount = F,
  AdvancedFemaleBlackCount = F,
  AdvancedFemaleHawaiianCount = F,
  AdvancedFemaleHispanicCount = F,
  AdvancedFemaleIDEACount = F,
  AdvancedFemaleLEPCount = F,
  AdvancedFemaleMultiRaceCount = F,
  AdvancedFemaleWhiteCount = F,
  AdvancedMaleAlaskanCount = F,
  AdvancedMaleAsianCount = F,
  AdvancedMaleBlackCount = F,
  AdvancedMaleHawaiianCount = F,
  AdvancedMaleHispanicCount = F,
  AdvancedMaleIDEACount = F,
  AdvancedMaleLEPCount = F,
  AdvancedMaleMultiRaceCount = F,
  AdvancedMaleWhiteCount = F,
  AlgebraICertifiedClassCount = F,
  AlgebraIICertifiedClassCount = F,
  AlgebraIIClassCount = F,
  AlgebraIIFemaleAlaskanCount = F,
  AlgebraIIFemaleAsianCount = F,
  AlgebraIIFemaleBlackCount = F,
  AlgebraIIFemaleHawaiianCount = F,
  AlgebraIIFemaleHispanicCount = F,
  AlgebraIIFemaleIDEACount = F,
  AlgebraIIFemaleLEPCount = F,
  AlgebraIIFemaleMultiRaceCount = F,
  AlgebraIIFemaleWhiteCount = F,
  AlgebraIIMaleAlaskanCount = F,
  AlgebraIIMaleAsianCount = F,
  AlgebraIIMaleBlackCount = F,
  AlgebraIIMaleHawaiianCount = F,
  AlgebraIIMaleHispanicCount = F,
  AlgebraIIMaleIDEACount = F,
  AlgebraIIMaleLEPCount = F,
  AlgebraIIMaleMultiRaceCount = F,
  AlgebraIIMaleWhiteCount = F,
  CalculusCertifiedClassCount = F,
  CalculusClassCount = F,
  CalculusFemaleAlaskanCount = F,
  CalculusFemaleAsianCount = F,
  CalculusFemaleBlackCount = F,
  CalculusFemaleHawaiianCount = F,
  CalculusFemaleHispanicCount = F,
  CalculusFemaleIDEACount = F,
  CalculusFemaleLEPCount = F,
  CalculusFemaleMultiRaceCount = F,
  CalculusFemaleWhiteCount = F,
  CalculusMaleAlaskanCount = F,
  CalculusMaleAsianCount = F,
  CalculusMaleBlackCount = F,
  CalculusMaleHawaiianCount = F,
  CalculusMaleHispanicCount = F,
  CalculusMaleIDEACount = F,
  CalculusMaleLEPCount = F,
  CalculusMaleMultiRaceCount = F,
  CalculusMaleWhiteCount = F,
  CivilRightsDataCollectionID = F,
  CivilRightsDataCollectionRunHistoryID = F,
  CreatedTime = F,
  FederalNCESSchoolID = F,
  GeometryCertifiedClassCount = F,
  ModifiedTime = F,
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
\item{CRDCSchoolAdvancedMathV1ID}{The ID of the CRDCSchoolAdvancedMathV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CRDCSchoolAdvancedMathV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given CRDCSchoolAdvancedMathV1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CRDCSchoolAdvancedMathV1.}
}
\value{
A dataframe or of CRDCSchoolAdvancedMathV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a CRDCSchoolAdvancedMathV1
}
\concept{StateReportingFederal}
