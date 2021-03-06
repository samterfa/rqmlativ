% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Discipline.R
\name{getTempIncidentOffenseNameActionWI}
\alias{getTempIncidentOffenseNameActionWI}
\title{Get a TempIncidentOffenseNameActionWI}
\usage{
getTempIncidentOffenseNameActionWI(
  TempIncidentOffenseNameActionWIID,
  ActionCodeDescription = F,
  ActionID = F,
  ActionTypeCode = F,
  ActionTypeID = F,
  BehaviorDetailedDescription = F,
  BuildingID = F,
  CausedSeriousBodilyInjury = F,
  Comment = F,
  CreatedTime = F,
  DurationToServe = F,
  DurationType = F,
  EntityID = F,
  FullName = F,
  HasEarlyReinstatementCondition = F,
  IAESRemovalType = F,
  InternalComment = F,
  InvolvementType = F,
  IsGuardianNotified = F,
  IsPrimaryOffense = F,
  LocationID = F,
  ModifiedTime = F,
  OffenseCodeDescription = F,
  OrderedDate = F,
  PerceivedMotivationCodeDescription = F,
  RoomID = F,
  StaffIDAuthorizedBy = F,
  StaffIDAuthorizedByName = F,
  StaffIDFollowUpOfficer = F,
  StartTime = F,
  StateModifiedTermWIID = F,
  Status = F,
  StudentNumber = F,
  TempIncidentOffenseNameActionID = F,
  TempIncidentOffenseNameID = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{TempIncidentOffenseNameActionWIID}{The ID of the TempIncidentOffenseNameActionWI to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameActionWI') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActionWI. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActionWI.}
}
\value{
A dataframe or of TempIncidentOffenseNameActionWI
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a TempIncidentOffenseNameActionWI
}
\concept{Discipline}
