% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReporting.R
\name{modifyDisciplineIncidentAssociationDashboardV1}
\alias{modifyDisciplineIncidentAssociationDashboardV1}
\title{Modify a DisciplineIncidentAssociationDashboardV1}
\usage{
modifyDisciplineIncidentAssociationDashboardV1(
  DisciplineIncidentAssociationDashboardV1ID,
  DashboardRunHistoryID = NULL,
  EntityID = NULL,
  IncidentID = NULL,
  IncidentIdentifier = NULL,
  SchoolYearID = NULL,
  StudentID = NULL,
  StudentParticipationCode = NULL,
  StudentUniqueID = NULL,
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

\item{fieldNames}{The field values to give the modified DisciplineIncidentAssociationDashboardV1. Each defaults to NULL.}
}
\value{
The modified DisciplineIncidentAssociationDashboardV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a DisciplineIncidentAssociationDashboardV1
}
\concept{StateReporting}
