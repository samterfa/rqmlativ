% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{modifyCRDCSchoolTeacherV1}
\alias{modifyCRDCSchoolTeacherV1}
\title{Modify a CRDCSchoolTeacherV1}
\usage{
modifyCRDCSchoolTeacherV1(
  CRDCSchoolTeacherV1ID,
  CertifiedFTETotal = NULL,
  CivilRightsDataCollectionID = NULL,
  CivilRightsDataCollectionRunHistoryID = NULL,
  CurrentYearCount = NULL,
  FederalNCESSchoolID = NULL,
  FirstYearFTETotal = NULL,
  FTETotal = NULL,
  NotCertifiedFTETotal = NULL,
  PreviousYearCount = NULL,
  SecondYearFTETotal = NULL,
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

\item{fieldNames}{The field values to give the modified CRDCSchoolTeacherV1. Each defaults to NULL.}
}
\value{
The modified CRDCSchoolTeacherV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a CRDCSchoolTeacherV1
}
\concept{StateReportingFederal}
