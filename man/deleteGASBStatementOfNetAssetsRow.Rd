% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReporting.R
\name{deleteGASBStatementOfNetAssetsRow}
\alias{deleteGASBStatementOfNetAssetsRow}
\title{Delete a GASBStatementOfNetAssetsRow}
\usage{
deleteGASBStatementOfNetAssetsRow(
  GASBStatementOfNetAssetsRowID,
  ignoreWarnings = F,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{GASBStatementOfNetAssetsRowID}{The ID of the GASBStatementOfNetAssetsRow to delete}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}
}
\value{
The GASBStatementOfNetAssetsRowID of the deleted GASBStatementOfNetAssetsRow.
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function deletes a GASBStatementOfNetAssetsRow
}
\concept{StateReporting}
