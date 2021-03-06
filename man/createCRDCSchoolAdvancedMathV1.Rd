% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{createCRDCSchoolAdvancedMathV1}
\alias{createCRDCSchoolAdvancedMathV1}
\title{Create a CRDCSchoolAdvancedMathV1}
\usage{
createCRDCSchoolAdvancedMathV1(
  AdvancedCertifiedClassCount = NULL,
  AdvancedClassCount = NULL,
  AdvancedFemaleAlaskanCount = NULL,
  AdvancedFemaleAsianCount = NULL,
  AdvancedFemaleBlackCount = NULL,
  AdvancedFemaleHawaiianCount = NULL,
  AdvancedFemaleHispanicCount = NULL,
  AdvancedFemaleIDEACount = NULL,
  AdvancedFemaleLEPCount = NULL,
  AdvancedFemaleMultiRaceCount = NULL,
  AdvancedFemaleWhiteCount = NULL,
  AdvancedMaleAlaskanCount = NULL,
  AdvancedMaleAsianCount = NULL,
  AdvancedMaleBlackCount = NULL,
  AdvancedMaleHawaiianCount = NULL,
  AdvancedMaleHispanicCount = NULL,
  AdvancedMaleIDEACount = NULL,
  AdvancedMaleLEPCount = NULL,
  AdvancedMaleMultiRaceCount = NULL,
  AdvancedMaleWhiteCount = NULL,
  AlgebraICertifiedClassCount = NULL,
  AlgebraIICertifiedClassCount = NULL,
  AlgebraIIClassCount = NULL,
  AlgebraIIFemaleAlaskanCount = NULL,
  AlgebraIIFemaleAsianCount = NULL,
  AlgebraIIFemaleBlackCount = NULL,
  AlgebraIIFemaleHawaiianCount = NULL,
  AlgebraIIFemaleHispanicCount = NULL,
  AlgebraIIFemaleIDEACount = NULL,
  AlgebraIIFemaleLEPCount = NULL,
  AlgebraIIFemaleMultiRaceCount = NULL,
  AlgebraIIFemaleWhiteCount = NULL,
  AlgebraIIMaleAlaskanCount = NULL,
  AlgebraIIMaleAsianCount = NULL,
  AlgebraIIMaleBlackCount = NULL,
  AlgebraIIMaleHawaiianCount = NULL,
  AlgebraIIMaleHispanicCount = NULL,
  AlgebraIIMaleIDEACount = NULL,
  AlgebraIIMaleLEPCount = NULL,
  AlgebraIIMaleMultiRaceCount = NULL,
  AlgebraIIMaleWhiteCount = NULL,
  CalculusCertifiedClassCount = NULL,
  CalculusClassCount = NULL,
  CalculusFemaleAlaskanCount = NULL,
  CalculusFemaleAsianCount = NULL,
  CalculusFemaleBlackCount = NULL,
  CalculusFemaleHawaiianCount = NULL,
  CalculusFemaleHispanicCount = NULL,
  CalculusFemaleIDEACount = NULL,
  CalculusFemaleLEPCount = NULL,
  CalculusFemaleMultiRaceCount = NULL,
  CalculusFemaleWhiteCount = NULL,
  CalculusMaleAlaskanCount = NULL,
  CalculusMaleAsianCount = NULL,
  CalculusMaleBlackCount = NULL,
  CalculusMaleHawaiianCount = NULL,
  CalculusMaleHispanicCount = NULL,
  CalculusMaleIDEACount = NULL,
  CalculusMaleLEPCount = NULL,
  CalculusMaleMultiRaceCount = NULL,
  CalculusMaleWhiteCount = NULL,
  CivilRightsDataCollectionID = NULL,
  CivilRightsDataCollectionRunHistoryID = NULL,
  FederalNCESSchoolID = NULL,
  GeometryCertifiedClassCount = NULL,
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

\item{fieldNames}{The field values to give the created CRDCSchoolAdvancedMathV1. Each defaults to NULL.}
}
\value{
A newly created CRDCSchoolAdvancedMathV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a CRDCSchoolAdvancedMathV1
}
\concept{StateReportingFederal}
