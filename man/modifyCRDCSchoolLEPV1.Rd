% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{modifyCRDCSchoolLEPV1}
\alias{modifyCRDCSchoolLEPV1}
\title{Modify a CRDCSchoolLEPV1}
\usage{
modifyCRDCSchoolLEPV1(
  CRDCSchoolLEPV1ID,
  CivilRightsDataCollectionID = NULL,
  CivilRightsDataCollectionRunHistoryID = NULL,
  EnrolledFemaleAlaskanCount = NULL,
  EnrolledFemaleAsianCount = NULL,
  EnrolledFemaleBlackCount = NULL,
  EnrolledFemaleHawaiianCount = NULL,
  EnrolledFemaleHispanicCount = NULL,
  EnrolledFemaleIDEACount = NULL,
  EnrolledFemaleMultiRaceCount = NULL,
  EnrolledFemaleWhiteCount = NULL,
  EnrolledMaleAlaskanCount = NULL,
  EnrolledMaleAsianCount = NULL,
  EnrolledMaleBlackCount = NULL,
  EnrolledMaleHawaiianCount = NULL,
  EnrolledMaleHispanicCount = NULL,
  EnrolledMaleIDEACount = NULL,
  EnrolledMaleMultiRaceCount = NULL,
  EnrolledMaleWhiteCount = NULL,
  FederalNCESSchoolID = NULL,
  StudentsFemaleAlaskanCount = NULL,
  StudentsFemaleAsianCount = NULL,
  StudentsFemaleBlackCount = NULL,
  StudentsFemaleHawaiianCount = NULL,
  StudentsFemaleHispanicCount = NULL,
  StudentsFemaleMultiRaceCount = NULL,
  StudentsFemaleWhiteCount = NULL,
  StudentsMaleAlaskanCount = NULL,
  StudentsMaleAsianCount = NULL,
  StudentsMaleBlackCount = NULL,
  StudentsMaleHawaiianCount = NULL,
  StudentsMaleHispanicCount = NULL,
  StudentsMaleMultiRaceCount = NULL,
  StudentsMaleWhiteCount = NULL,
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

\item{fieldNames}{The field values to give the modified CRDCSchoolLEPV1. Each defaults to NULL.}
}
\value{
The modified CRDCSchoolLEPV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a CRDCSchoolLEPV1
}
\concept{StateReportingFederal}
