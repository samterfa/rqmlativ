% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{deleteVCRSection3V1}
\alias{deleteVCRSection3V1}
\title{Delete a VCRSection3V1}
\usage{
deleteVCRSection3V1(
  VCRSection3V1ID,
  ignoreWarnings = F,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{VCRSection3V1ID}{The ID of the VCRSection3V1 to delete}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}
}
\value{
The VCRSection3V1ID of the deleted VCRSection3V1.
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function deletes a VCRSection3V1
}
\concept{StateReportingFederal}
