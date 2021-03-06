% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{getCRDCSchoolDisciplineWithDisabilityV1}
\alias{getCRDCSchoolDisciplineWithDisabilityV1}
\title{Get a CRDCSchoolDisciplineWithDisabilityV1}
\usage{
getCRDCSchoolDisciplineWithDisabilityV1(
  CRDCSchoolDisciplineWithDisabilityV1ID,
  ArrestFemaleAlaskanCount = F,
  ArrestFemaleAsianCount = F,
  ArrestFemaleBlackCount = F,
  ArrestFemaleHawaiianCount = F,
  ArrestFemaleHispanicCount = F,
  ArrestFemaleLEPCount = F,
  ArrestFemaleMultiRaceCount = F,
  ArrestFemaleSection504Count = F,
  ArrestFemaleWhiteCount = F,
  ArrestMaleAlaskanCount = F,
  ArrestMaleAsianCount = F,
  ArrestMaleBlackCount = F,
  ArrestMaleHawaiianCount = F,
  ArrestMaleHispanicCount = F,
  ArrestMaleLEPCount = F,
  ArrestMaleMultiRaceCount = F,
  ArrestMaleSection504Count = F,
  ArrestMaleWhiteCount = F,
  CivilRightsDataCollectionID = F,
  CivilRightsDataCollectionRunHistoryID = F,
  CorporalFemaleAlaskanCount = F,
  CorporalFemaleAsianCount = F,
  CorporalFemaleBlackCount = F,
  CorporalFemaleHawaiianCount = F,
  CorporalFemaleHispanicCount = F,
  CorporalFemaleLEPCount = F,
  CorporalFemaleMultiRaceCount = F,
  CorporalFemaleSection504Count = F,
  CorporalFemaleWhiteCount = F,
  CorporalMaleAlaskanCount = F,
  CorporalMaleAsianCount = F,
  CorporalMaleBlackCount = F,
  CorporalMaleHawaiianCount = F,
  CorporalMaleHispanicCount = F,
  CorporalMaleLEPCount = F,
  CorporalMaleMultiRaceCount = F,
  CorporalMaleSection504Count = F,
  CorporalMaleWhiteCount = F,
  CreatedTime = F,
  ExpulsionNoServicesFemaleAlaskanCount = F,
  ExpulsionNoServicesFemaleAsianCount = F,
  ExpulsionNoServicesFemaleBlackCount = F,
  ExpulsionNoServicesFemaleHawaiianCount = F,
  ExpulsionNoServicesFemaleHispanicCount = F,
  ExpulsionNoServicesFemaleLEPCount = F,
  ExpulsionNoServicesFemaleMultiRaceCount = F,
  ExpulsionNoServicesFemaleSection504Count = F,
  ExpulsionNoServicesFemaleWhiteCount = F,
  ExpulsionNoServicesMaleAlaskanCount = F,
  ExpulsionNoServicesMaleAsianCount = F,
  ExpulsionNoServicesMaleBlackCount = F,
  ExpulsionNoServicesMaleHawaiianCount = F,
  ExpulsionNoServicesMaleHispanicCount = F,
  ExpulsionNoServicesMaleLEPCount = F,
  ExpulsionNoServicesMaleMultiRaceCount = F,
  ExpulsionNoServicesMaleSection504Count = F,
  ExpulsionNoServicesMaleWhiteCount = F,
  ExpulsionWithServicesFemaleAlaskanCount = F,
  ExpulsionWithServicesFemaleAsianCount = F,
  ExpulsionWithServicesFemaleBlackCount = F,
  ExpulsionWithServicesFemaleHawaiianCount = F,
  ExpulsionWithServicesFemaleHispanicCount = F,
  ExpulsionWithServicesFemaleLEPCount = F,
  ExpulsionWithServicesFemaleMultiRaceCount = F,
  ExpulsionWithServicesFemaleSection504Count = F,
  ExpulsionWithServicesFemaleWhiteCount = F,
  ExpulsionWithServicesMaleAlaskanCount = F,
  ExpulsionWithServicesMaleAsianCount = F,
  ExpulsionWithServicesMaleBlackCount = F,
  ExpulsionWithServicesMaleHawaiianCount = F,
  ExpulsionWithServicesMaleHispanicCount = F,
  ExpulsionWithServicesMaleLEPCount = F,
  ExpulsionWithServicesMaleMultiRaceCount = F,
  ExpulsionWithServicesMaleSection504Count = F,
  ExpulsionWithServicesMaleWhiteCount = F,
  FederalNCESSchoolID = F,
  InSchoolFemaleAlaskanCount = F,
  InSchoolFemaleAsianCount = F,
  InSchoolFemaleBlackCount = F,
  InSchoolFemaleHawaiianCount = F,
  InSchoolFemaleHispanicCount = F,
  InSchoolFemaleLEPCount = F,
  InSchoolFemaleMultiRaceCount = F,
  InSchoolFemaleSection504Count = F,
  InSchoolFemaleWhiteCount = F,
  InSchoolMaleAlaskanCount = F,
  InSchoolMaleAsianCount = F,
  InSchoolMaleBlackCount = F,
  InSchoolMaleHawaiianCount = F,
  InSchoolMaleHispanicCount = F,
  InSchoolMaleLEPCount = F,
  InSchoolMaleMultiRaceCount = F,
  InSchoolMaleSection504Count = F,
  InSchoolMaleWhiteCount = F,
  LawEnforcementFemaleAlaskanCount = F,
  LawEnforcementFemaleAsianCount = F,
  LawEnforcementFemaleBlackCount = F,
  LawEnforcementFemaleHawaiianCount = F,
  LawEnforcementFemaleHispanicCount = F,
  LawEnforcementFemaleLEPCount = F,
  LawEnforcementFemaleMultiRaceCount = F,
  LawEnforcementFemaleSection504Count = F,
  LawEnforcementFemaleWhiteCount = F,
  LawEnforcementMaleAlaskanCount = F,
  LawEnforcementMaleAsianCount = F,
  LawEnforcementMaleBlackCount = F,
  LawEnforcementMaleHawaiianCount = F,
  LawEnforcementMaleHispanicCount = F,
  LawEnforcementMaleLEPCount = F,
  LawEnforcementMaleMultiRaceCount = F,
  LawEnforcementMaleSection504Count = F,
  LawEnforcementMaleWhiteCount = F,
  ModifiedTime = F,
  MultipleOutOfSchoolFemaleAlaskanCount = F,
  MultipleOutOfSchoolFemaleAsianCount = F,
  MultipleOutOfSchoolFemaleBlackCount = F,
  MultipleOutOfSchoolFemaleHawaiianCount = F,
  MultipleOutOfSchoolFemaleHispanicCount = F,
  MultipleOutOfSchoolFemaleLEPCount = F,
  MultipleOutOfSchoolFemaleMultiRaceCount = F,
  MultipleOutOfSchoolFemaleSection504Count = F,
  MultipleOutOfSchoolFemaleWhiteCount = F,
  MultipleOutOfSchoolMaleAlaskanCount = F,
  MultipleOutOfSchoolMaleAsianCount = F,
  MultipleOutOfSchoolMaleBlackCount = F,
  MultipleOutOfSchoolMaleHawaiianCount = F,
  MultipleOutOfSchoolMaleHispanicCount = F,
  MultipleOutOfSchoolMaleLEPCount = F,
  MultipleOutOfSchoolMaleMultiRaceCount = F,
  MultipleOutOfSchoolMaleSection504Count = F,
  MultipleOutOfSchoolMaleWhiteCount = F,
  OneOutOfSchoolFemaleAlaskanCount = F,
  OneOutOfSchoolFemaleAsianCount = F,
  OneOutOfSchoolFemaleBlackCount = F,
  OneOutOfSchoolFemaleHawaiianCount = F,
  OneOutOfSchoolFemaleHispanicCount = F,
  OneOutOfSchoolFemaleLEPCount = F,
  OneOutOfSchoolFemaleMultiRaceCount = F,
  OneOutOfSchoolFemaleSection504Count = F,
  OneOutOfSchoolFemaleWhiteCount = F,
  OneOutOfSchoolMaleAlaskanCount = F,
  OneOutOfSchoolMaleAsianCount = F,
  OneOutOfSchoolMaleBlackCount = F,
  OneOutOfSchoolMaleHawaiianCount = F,
  OneOutOfSchoolMaleHispanicCount = F,
  OneOutOfSchoolMaleLEPCount = F,
  OneOutOfSchoolMaleMultiRaceCount = F,
  OneOutOfSchoolMaleSection504Count = F,
  OneOutOfSchoolMaleWhiteCount = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  ZeroToleranceFemaleAlaskanCount = F,
  ZeroToleranceFemaleAsianCount = F,
  ZeroToleranceFemaleBlackCount = F,
  ZeroToleranceFemaleHawaiianCount = F,
  ZeroToleranceFemaleHispanicCount = F,
  ZeroToleranceFemaleLEPCount = F,
  ZeroToleranceFemaleMultiRaceCount = F,
  ZeroToleranceFemaleSection504Count = F,
  ZeroToleranceFemaleWhiteCount = F,
  ZeroToleranceMaleAlaskanCount = F,
  ZeroToleranceMaleAsianCount = F,
  ZeroToleranceMaleBlackCount = F,
  ZeroToleranceMaleHawaiianCount = F,
  ZeroToleranceMaleHispanicCount = F,
  ZeroToleranceMaleLEPCount = F,
  ZeroToleranceMaleMultiRaceCount = F,
  ZeroToleranceMaleSection504Count = F,
  ZeroToleranceMaleWhiteCount = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{CRDCSchoolDisciplineWithDisabilityV1ID}{The ID of the CRDCSchoolDisciplineWithDisabilityV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CRDCSchoolDisciplineWithDisabilityV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given CRDCSchoolDisciplineWithDisabilityV1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CRDCSchoolDisciplineWithDisabilityV1.}
}
\value{
A dataframe or of CRDCSchoolDisciplineWithDisabilityV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a CRDCSchoolDisciplineWithDisabilityV1
}
\concept{StateReportingFederal}
