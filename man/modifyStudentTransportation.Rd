% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Transportation.R
\name{modifyStudentTransportation}
\alias{modifyStudentTransportation}
\title{Modify a StudentTransportation}
\usage{
modifyStudentTransportation(
  StudentTransportationID,
  DistrictID = NULL,
  EndDate = NULL,
  Miles = NULL,
  SchoolYearID = NULL,
  StartDate = NULL,
  StateDistrictMNID = NULL,
  StudentID = NULL,
  TransportationCategoryID = NULL,
  Transported = NULL,
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

\item{fieldNames}{The field values to give the modified StudentTransportation. Each defaults to NULL.}
}
\value{
The modified StudentTransportation
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a StudentTransportation
}
\concept{Transportation}
