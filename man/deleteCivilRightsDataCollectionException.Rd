% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{deleteCivilRightsDataCollectionException}
\alias{deleteCivilRightsDataCollectionException}
\title{Delete a CivilRightsDataCollectionException}
\usage{
deleteCivilRightsDataCollectionException(
  CivilRightsDataCollectionExceptionID,
  ignoreWarnings = F,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{CivilRightsDataCollectionExceptionID}{The ID of the CivilRightsDataCollectionException to delete}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}
}
\value{
The CivilRightsDataCollectionExceptionID of the deleted CivilRightsDataCollectionException.
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function deletes a CivilRightsDataCollectionException
}
\concept{StateReportingFederal}
