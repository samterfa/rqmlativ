% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Demographics.R
\name{getNameAlias}
\alias{getNameAlias}
\title{Get a NameAlias}
\usage{
getNameAlias(
  NameAliasID,
  CreatedTime = F,
  EffectiveDate = F,
  FirstName = F,
  FullNameFL = F,
  FullNameLF = F,
  IsBusiness = F,
  IsLegalName = F,
  IsMaidenName = F,
  IsSingleLegalName = F,
  LastName = F,
  MiddleName = F,
  ModifiedTime = F,
  NameID = F,
  NameSuffixID = F,
  NameTitleID = F,
  Rank = F,
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
\item{NameAliasID}{The ID of the NameAlias to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NameAlias') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given NameAlias. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NameAlias.}
}
\value{
A dataframe or of NameAlias
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a NameAlias
}
\concept{Demographics}
