% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReporting.R
\name{deleteStudentTitleIPartAProgramDashboardV1}
\alias{deleteStudentTitleIPartAProgramDashboardV1}
\title{Delete a StudentTitleIPartAProgramDashboardV1}
\usage{
deleteStudentTitleIPartAProgramDashboardV1(
  StudentTitleIPartAProgramDashboardV1ID,
  ignoreWarnings = F,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{StudentTitleIPartAProgramDashboardV1ID}{The ID of the StudentTitleIPartAProgramDashboardV1 to delete}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}
}
\value{
The StudentTitleIPartAProgramDashboardV1ID of the deleted StudentTitleIPartAProgramDashboardV1.
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function deletes a StudentTitleIPartAProgramDashboardV1
}
\concept{StateReporting}
