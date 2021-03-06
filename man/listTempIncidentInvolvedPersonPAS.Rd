% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Discipline.R
\name{listTempIncidentInvolvedPersonPAS}
\alias{listTempIncidentInvolvedPersonPAS}
\title{List TempIncidentInvolvedPersonPAS}
\usage{
listTempIncidentInvolvedPersonPAS(
  searchConditionsList = NULL,
  AgeAtTimeOfIncident = F,
  CreatedTime = F,
  ExcludePrimaryOffense = F,
  ExistingIncidentOffenseNamesToDelete = F,
  FirstName = F,
  FreeformName = F,
  FullName = F,
  Genders = F,
  IncidentOffenseNameKey = F,
  IncidentOffenseNameType = F,
  IncidentVictimComment = F,
  InternalComment = F,
  InvolvementType = F,
  IsResidentialPlacementByNonEdAgency = F,
  ISSCount = F,
  ISSPartialCount = F,
  LastName = F,
  LLEIncidentNumber = F,
  LocalLawEnforcementNotified = F,
  MedicalTreatmentRequired = F,
  ModifiedTime = F,
  NameID = F,
  NameOfLocalLawEnforcementContacted = F,
  OSSCount = F,
  OSSPartialCount = F,
  PASecureID = F,
  PerceivedMotivationCodeDescription = F,
  PerceivedMotivationID = F,
  PrimaryOffenseID = F,
  RaceEthnicities = F,
  SecondaryOffensesBackingData = F,
  SendingDistrictOrCharterAUNID = F,
  SendingOrCharterTypes = F,
  StaffID = F,
  StaffIDDisciplineOfficer = F,
  StateAdjudicationPAID = F,
  StateArrestedPAID = F,
  StateDistrictPAIDPerson = F,
  StateGradeLevelPIMSPAID = F,
  StateInjurySeverityPAID = F,
  StateOffenderTypePAID = F,
  StateVictimTypePAID = F,
  StateWeaponDetectedMethodPAID = F,
  StudentAssistanceProgramReferral = F,
  StudentID = F,
  StudentNumber = F,
  TempIncidentInvolvedPersonID = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  WeaponDetectionComment = F,
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

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentInvolvedPersonPA') to get more field paths.}

\item{searchConditionsGroupType}{The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.}

\item{searchSortFieldNamesList}{The list of fields sort results by. Defaults to NULL (unsorted).}

\item{searchSortFieldNamesDescendingList}{A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{page}{Results are paginated. The page of results to return. Default is 1.}

\item{pageSize}{Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).}

\item{flatten}{Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentInvolvedPersonPAS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentInvolvedPersonPAS.}
}
\value{
A list of TempIncidentInvolvedPersonPAS
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of TempIncidentInvolvedPersonPAS
}
\concept{Discipline}
