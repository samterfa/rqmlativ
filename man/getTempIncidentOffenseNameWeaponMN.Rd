% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Discipline.R
\name{getTempIncidentOffenseNameWeaponMN}
\alias{getTempIncidentOffenseNameWeaponMN}
\title{Get a TempIncidentOffenseNameWeaponMN}
\usage{
getTempIncidentOffenseNameWeaponMN(
  TempIncidentOffenseNameWeaponMNID,
  CreatedTime = F,
  GunFoundInTrunk = F,
  GunWasInCase = F,
  GunWasLoaded = F,
  IncidentOffenseNameWeaponID = F,
  MeetsFederalStatuteOfDangerousWeapon = F,
  MeetsStateStatuteOfDangerousWeapon = F,
  ModifiedTime = F,
  TempIncidentOffenseNameID = F,
  TempIncidentOffenseNameWeaponID = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  WeaponCount = F,
  WeaponID = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{TempIncidentOffenseNameWeaponMNID}{The ID of the TempIncidentOffenseNameWeaponMN to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameWeaponMN') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameWeaponMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameWeaponMN.}
}
\value{
A dataframe or of TempIncidentOffenseNameWeaponMN
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a TempIncidentOffenseNameWeaponMN
}
\concept{Discipline}
