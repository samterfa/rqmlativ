% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReporting.R
\name{listStudentDashboardV1s}
\alias{listStudentDashboardV1s}
\title{List StudentDashboardV1s}
\usage{
listStudentDashboardV1s(
  searchConditionsList = NULL,
  ApartmentNumberHome = F,
  ApartmentNumberMailing = F,
  BirthCity = F,
  BirthCountryCode = F,
  BirthDate = F,
  BirthStateCode = F,
  BuildingNumberHome = F,
  BuildingNumberMailing = F,
  CityHome = F,
  CityMailing = F,
  CountyHome = F,
  CountyMailing = F,
  CreatedTime = F,
  DashboardRunHistoryID = F,
  DisabilityTypeCode = F,
  DistrictCode = F,
  EdFiCountryID = F,
  EdFiDisabilityTypeID = F,
  EdFiGenerationSuffixID = F,
  EdFiLanguageID = F,
  EdFiLanguageIDHome = F,
  EdFiLimitedEnglishProficiencyID = F,
  EdFiSchoolFoodServicesEligibilityID = F,
  EdFiSexID = F,
  EdFiStateAbbreviationIDBirth = F,
  EdFiStateAbbreviationIDHome = F,
  EdFiStateAbbreviationIDMailing = F,
  EmailAddress = F,
  EntityID = F,
  EntryWithdrawalID = F,
  FirstName = F,
  GenerationSuffixCode = F,
  HomeLanguageCode = F,
  IsAlaskan = F,
  IsAsian = F,
  IsAtRisk = F,
  IsBlack = F,
  IsDisplacedHomemaker = F,
  IsEconomicDisadvantaged = F,
  IsHawaiian = F,
  IsHispanic = F,
  IsHomeless = F,
  IsImmigrant = F,
  IsMigrant = F,
  IsPregnantTeen = F,
  IsSingleParent = F,
  IsWhite = F,
  KeyHash = F,
  LanguageCode = F,
  LastName = F,
  LimitedEnglishProficiencyCode = F,
  LocalDistrictStudentID = F,
  LocalStateStudentID = F,
  MiddleName = F,
  ModifiedTime = F,
  PostalCodeHome = F,
  PostalCodeMailing = F,
  SchoolFoodServicesEligibilityCode = F,
  SchoolYearID = F,
  SexCode = F,
  StateCodeHome = F,
  StateCodeMailing = F,
  StreetNumberNameHome = F,
  StreetNumberNameMailing = F,
  StudentDashboardV1ID = F,
  StudentID = F,
  StudentUniqueID = F,
  TelephoneNumber = F,
  UpdateHash = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
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

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentDashboardV1') to get more field paths.}

\item{searchConditionsGroupType}{The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.}

\item{searchSortFieldNamesList}{The list of fields sort results by. Defaults to NULL (unsorted).}

\item{searchSortFieldNamesDescendingList}{A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{page}{Results are paginated. The page of results to return. Default is 1.}

\item{pageSize}{Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).}

\item{flatten}{Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given StudentDashboardV1s. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentDashboardV1s.}
}
\value{
A list of StudentDashboardV1s
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of StudentDashboardV1s
}
\concept{StateReporting}
