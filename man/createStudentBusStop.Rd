% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Transportation.R
\name{createStudentBusStop}
\alias{createStudentBusStop}
\title{Create a StudentBusStop}
\usage{
createStudentBusStop(
  BusStopID = NULL,
  IsFriday = NULL,
  IsMonday = NULL,
  IsSaturday = NULL,
  IsSunday = NULL,
  IsThursday = NULL,
  IsTuesday = NULL,
  IsWednesday = NULL,
  StopType = NULL,
  StudentID = NULL,
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

\item{fieldNames}{The field values to give the created StudentBusStop. Each defaults to NULL.}
}
\value{
A newly created StudentBusStop
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a StudentBusStop
}
\concept{Transportation}
