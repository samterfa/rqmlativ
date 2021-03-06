% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Demographics.R
\name{getTempNameEmail}
\alias{getTempNameEmail}
\title{Get a TempNameEmail}
\usage{
getTempNameEmail(
  TempNameEmailID,
  CreatedTime = F,
  EmailAddress = F,
  EmailTypeID = F,
  ErrorCount = F,
  FullNameFML = F,
  HasError = F,
  ModifiedTime = F,
  NameEmailID = F,
  NameID = F,
  Note = F,
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
\item{TempNameEmailID}{The ID of the TempNameEmail to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNameEmail') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given TempNameEmail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNameEmail.}
}
\value{
A dataframe or of TempNameEmail
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a TempNameEmail
}
\concept{Demographics}
