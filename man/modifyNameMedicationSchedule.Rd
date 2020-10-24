% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Health.R
\name{modifyNameMedicationSchedule}
\alias{modifyNameMedicationSchedule}
\title{Modify a NameMedicationSchedule}
\usage{
modifyNameMedicationSchedule(
  NameMedicationScheduleID,
  NameMedicationID = NULL,
  StartDate = NULL,
  EndDate = NULL,
  ScheduledAdministrationTime = NULL,
  Sunday = NULL,
  Monday = NULL,
  Tuesday = NULL,
  Wednesday = NULL,
  Thursday = NULL,
  Friday = NULL,
  Saturday = NULL,
  IsVoid = NULL,
  VoidNote = NULL,
  UserIDVoidedBy = NULL,
  VoidedTime = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{The field values to give the modified NameMedicationSchedule. Each defaults to NULL.}
}
\value{
The modified NameMedicationSchedule
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a NameMedicationSchedule
}
\concept{Health}