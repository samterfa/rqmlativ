% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Security.R
\name{getTempFamilyAccessSecurityUser}
\alias{getTempFamilyAccessSecurityUser}
\title{Get a TempFamilyAccessSecurityUser}
\usage{
getTempFamilyAccessSecurityUser(
  TempFamilyAccessSecurityUserID,
  AddToFamilyAccess = F,
  CreatedTime = F,
  EmailAddress = F,
  EntityCodeName = F,
  ForUserCreation = F,
  GuardianNameLFM = F,
  IsAuditFamilyAccessSecurity = F,
  IsException = F,
  IsSelected = F,
  ModifiedTime = F,
  RemoveFromFamilyAccess = F,
  StudentGuardianID = F,
  StudentNameLFM = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  UserName = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{TempFamilyAccessSecurityUserID}{The ID of the TempFamilyAccessSecurityUser to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFamilyAccessSecurityUser') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given TempFamilyAccessSecurityUser. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFamilyAccessSecurityUser.}
}
\value{
A dataframe or of TempFamilyAccessSecurityUser
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a TempFamilyAccessSecurityUser
}
\concept{Security}
