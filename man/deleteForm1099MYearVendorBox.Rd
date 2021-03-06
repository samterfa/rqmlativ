% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{deleteForm1099MYearVendorBox}
\alias{deleteForm1099MYearVendorBox}
\title{Delete a Form1099MYearVendorBox}
\usage{
deleteForm1099MYearVendorBox(
  Form1099MYearVendorBoxID,
  ignoreWarnings = F,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{Form1099MYearVendorBoxID}{The ID of the Form1099MYearVendorBox to delete}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}
}
\value{
The Form1099MYearVendorBoxID of the deleted Form1099MYearVendorBox.
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function deletes a Form1099MYearVendorBox
}
\concept{StateReportingFederal}
