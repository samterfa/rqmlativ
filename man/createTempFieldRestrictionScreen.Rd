% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Security.R
\name{createTempFieldRestrictionScreen}
\alias{createTempFieldRestrictionScreen}
\title{Create a TempFieldRestrictionScreen}
\usage{
createTempFieldRestrictionScreen(
  SecurityLocationID = NULL,
  DisplayText = NULL,
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

\item{fieldNames}{The field values to give the created TempFieldRestrictionScreen. Each defaults to NULL.}
}
\value{
A newly created TempFieldRestrictionScreen
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a TempFieldRestrictionScreen
}
\concept{Security}