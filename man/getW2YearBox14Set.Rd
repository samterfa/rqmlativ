% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{getW2YearBox14Set}
\alias{getW2YearBox14Set}
\title{Get a W2YearBox14Set}
\usage{
getW2YearBox14Set(
  W2YearBox14SetID,
  CreatedTime = F,
  ModifiedTime = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  W2Box14SetID = F,
  W2Description = F,
  W2YearID = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{W2YearBox14SetID}{The ID of the W2YearBox14Set to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('W2YearBox14Set') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given W2YearBox14Set. Defaults to FALSE for all return fields which, for convenience, returns all fields for the W2YearBox14Set.}
}
\value{
A dataframe or of W2YearBox14Set
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a W2YearBox14Set
}
\concept{StateReportingFederal}
