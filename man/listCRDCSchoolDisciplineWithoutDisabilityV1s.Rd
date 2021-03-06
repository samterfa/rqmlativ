% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{listCRDCSchoolDisciplineWithoutDisabilityV1s}
\alias{listCRDCSchoolDisciplineWithoutDisabilityV1s}
\title{List CRDCSchoolDisciplineWithoutDisabilityV1s}
\usage{
listCRDCSchoolDisciplineWithoutDisabilityV1s(
  searchConditionsList = NULL,
  ArrestFemaleAlaskanCount = F,
  ArrestFemaleAsianCount = F,
  ArrestFemaleBlackCount = F,
  ArrestFemaleHawaiianCount = F,
  ArrestFemaleHispanicCount = F,
  ArrestFemaleLEPCount = F,
  ArrestFemaleMultiRaceCount = F,
  ArrestFemaleWhiteCount = F,
  ArrestMaleAlaskanCount = F,
  ArrestMaleAsianCount = F,
  ArrestMaleBlackCount = F,
  ArrestMaleHawaiianCount = F,
  ArrestMaleHispanicCount = F,
  ArrestMaleLEPCount = F,
  ArrestMaleMultiRaceCount = F,
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
  CorporalFemaleWhiteCount = F,
  CorporalMaleAlaskanCount = F,
  CorporalMaleAsianCount = F,
  CorporalMaleBlackCount = F,
  CorporalMaleHawaiianCount = F,
  CorporalMaleHispanicCount = F,
  CorporalMaleLEPCount = F,
  CorporalMaleMultiRaceCount = F,
  CorporalMaleWhiteCount = F,
  CRDCSchoolDisciplineWithoutDisabilityV1ID = F,
  CreatedTime = F,
  ExpulsionNoServicesFemaleAlaskanCount = F,
  ExpulsionNoServicesFemaleAsianCount = F,
  ExpulsionNoServicesFemaleBlackCount = F,
  ExpulsionNoServicesFemaleHawaiianCount = F,
  ExpulsionNoServicesFemaleHispanicCount = F,
  ExpulsionNoServicesFemaleLEPCount = F,
  ExpulsionNoServicesFemaleMultiRaceCount = F,
  ExpulsionNoServicesFemaleWhiteCount = F,
  ExpulsionNoServicesMaleAlaskanCount = F,
  ExpulsionNoServicesMaleAsianCount = F,
  ExpulsionNoServicesMaleBlackCount = F,
  ExpulsionNoServicesMaleHawaiianCount = F,
  ExpulsionNoServicesMaleHispanicCount = F,
  ExpulsionNoServicesMaleLEPCount = F,
  ExpulsionNoServicesMaleMultiRaceCount = F,
  ExpulsionNoServicesMaleWhiteCount = F,
  ExpulsionWithServicesFemaleAlaskanCount = F,
  ExpulsionWithServicesFemaleAsianCount = F,
  ExpulsionWithServicesFemaleBlackCount = F,
  ExpulsionWithServicesFemaleHawaiianCount = F,
  ExpulsionWithServicesFemaleHispanicCount = F,
  ExpulsionWithServicesFemaleLEPCount = F,
  ExpulsionWithServicesFemaleMultiRaceCount = F,
  ExpulsionWithServicesFemaleWhiteCount = F,
  ExpulsionWithServicesMaleAlaskanCount = F,
  ExpulsionWithServicesMaleAsianCount = F,
  ExpulsionWithServicesMaleBlackCount = F,
  ExpulsionWithServicesMaleHawaiianCount = F,
  ExpulsionWithServicesMaleHispanicCount = F,
  ExpulsionWithServicesMaleLEPCount = F,
  ExpulsionWithServicesMaleMultiRaceCount = F,
  ExpulsionWithServicesMaleWhiteCount = F,
  FederalNCESSchoolID = F,
  HasCorporalPunishment = F,
  InSchoolFemaleAlaskanCount = F,
  InSchoolFemaleAsianCount = F,
  InSchoolFemaleBlackCount = F,
  InSchoolFemaleHawaiianCount = F,
  InSchoolFemaleHispanicCount = F,
  InSchoolFemaleLEPCount = F,
  InSchoolFemaleMultiRaceCount = F,
  InSchoolFemaleWhiteCount = F,
  InSchoolMaleAlaskanCount = F,
  InSchoolMaleAsianCount = F,
  InSchoolMaleBlackCount = F,
  InSchoolMaleHawaiianCount = F,
  InSchoolMaleHispanicCount = F,
  InSchoolMaleLEPCount = F,
  InSchoolMaleMultiRaceCount = F,
  InSchoolMaleWhiteCount = F,
  LawEnforcementFemaleAlaskanCount = F,
  LawEnforcementFemaleAsianCount = F,
  LawEnforcementFemaleBlackCount = F,
  LawEnforcementFemaleHawaiianCount = F,
  LawEnforcementFemaleHispanicCount = F,
  LawEnforcementFemaleLEPCount = F,
  LawEnforcementFemaleMultiRaceCount = F,
  LawEnforcementFemaleWhiteCount = F,
  LawEnforcementMaleAlaskanCount = F,
  LawEnforcementMaleAsianCount = F,
  LawEnforcementMaleBlackCount = F,
  LawEnforcementMaleHawaiianCount = F,
  LawEnforcementMaleHispanicCount = F,
  LawEnforcementMaleLEPCount = F,
  LawEnforcementMaleMultiRaceCount = F,
  LawEnforcementMaleWhiteCount = F,
  ModifiedTime = F,
  MultipleOutOfSchoolFemaleAlaskanCount = F,
  MultipleOutOfSchoolFemaleAsianCount = F,
  MultipleOutOfSchoolFemaleBlackCount = F,
  MultipleOutOfSchoolFemaleHawaiianCount = F,
  MultipleOutOfSchoolFemaleHispanicCount = F,
  MultipleOutOfSchoolFemaleLEPCount = F,
  MultipleOutOfSchoolFemaleMultiRaceCount = F,
  MultipleOutOfSchoolFemaleWhiteCount = F,
  MultipleOutOfSchoolMaleAlaskanCount = F,
  MultipleOutOfSchoolMaleAsianCount = F,
  MultipleOutOfSchoolMaleBlackCount = F,
  MultipleOutOfSchoolMaleHawaiianCount = F,
  MultipleOutOfSchoolMaleHispanicCount = F,
  MultipleOutOfSchoolMaleLEPCount = F,
  MultipleOutOfSchoolMaleMultiRaceCount = F,
  MultipleOutOfSchoolMaleWhiteCount = F,
  OneOutOfSchoolFemaleAlaskanCount = F,
  OneOutOfSchoolFemaleAsianCount = F,
  OneOutOfSchoolFemaleBlackCount = F,
  OneOutOfSchoolFemaleHawaiianCount = F,
  OneOutOfSchoolFemaleHispanicCount = F,
  OneOutOfSchoolFemaleLEPCount = F,
  OneOutOfSchoolFemaleMultiRaceCount = F,
  OneOutOfSchoolFemaleWhiteCount = F,
  OneOutOfSchoolMaleAlaskanCount = F,
  OneOutOfSchoolMaleAsianCount = F,
  OneOutOfSchoolMaleBlackCount = F,
  OneOutOfSchoolMaleHawaiianCount = F,
  OneOutOfSchoolMaleHispanicCount = F,
  OneOutOfSchoolMaleLEPCount = F,
  OneOutOfSchoolMaleMultiRaceCount = F,
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
  ZeroToleranceFemaleWhiteCount = F,
  ZeroToleranceMaleAlaskanCount = F,
  ZeroToleranceMaleAsianCount = F,
  ZeroToleranceMaleBlackCount = F,
  ZeroToleranceMaleHawaiianCount = F,
  ZeroToleranceMaleHispanicCount = F,
  ZeroToleranceMaleLEPCount = F,
  ZeroToleranceMaleMultiRaceCount = F,
  ZeroToleranceMaleWhiteCount = F,
  fieldPaths = NULL,
  searchConditionsGroupType = "And",
  searchSortFieldNamesList = NULL,
  searchSortFieldNamesDescendingList = NULL,
  entityId = 1,
  query = NULL,
  page = 1,
  pageSize = 1e+05,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{searchConditionsList}{A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CRDCSchoolDisciplineWithoutDisabilityV1') to get more field paths.}

\item{searchConditionsGroupType}{The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.}

\item{searchSortFieldNamesList}{The list of fields sort results by. Defaults to NULL (unsorted).}

\item{searchSortFieldNamesDescendingList}{A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{page}{Results are paginated. The page of results to return. Default is 1.}

\item{pageSize}{Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).}

\item{flatten}{Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given CRDCSchoolDisciplineWithoutDisabilityV1s. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CRDCSchoolDisciplineWithoutDisabilityV1s.}
}
\value{
A list of CRDCSchoolDisciplineWithoutDisabilityV1s
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of CRDCSchoolDisciplineWithoutDisabilityV1s
}
\concept{StateReportingFederal}
