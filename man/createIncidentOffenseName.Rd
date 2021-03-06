% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Discipline.R
\name{createIncidentOffenseName}
\alias{createIncidentOffenseName}
\title{Create an IncidentOffenseName}
\usage{
createIncidentOffenseName(
  DisciplineThresholdID = NULL,
  EstimatedVictimsEnrolled = NULL,
  EstimatedVictimsNotEnrolled = NULL,
  FreeformName = NULL,
  IncidentOffenseID = NULL,
  IncidentOffenseNameType = NULL,
  InjuryOccured = NULL,
  InvolvementType = NULL,
  IsGuardianNotified = NULL,
  IsPhysicalAssault = NULL,
  IsPhysicalAssaultState = NULL,
  MultipleVictimCount = NULL,
  NameID = NULL,
  OffenderArrestedByLawEnforcement = NULL,
  OffenseLevelID = NULL,
  PerceivedMotivationID = NULL,
  ReportedToLawEnforcement = NULL,
  StaffIDDisciplineOfficer = NULL,
  Statement = NULL,
  StateOffenderActivityMNID = NULL,
  StateVictimCostMNID = NULL,
  StateVictimTypeMNID = NULL,
  WasSeriousBodilyInjury = NULL,
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

\item{fieldNames}{The field values to give the created IncidentOffenseName. Each defaults to NULL.}
}
\value{
A newly created IncidentOffenseName
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates an IncidentOffenseName
}
\concept{Discipline}
