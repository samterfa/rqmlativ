% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{getFederalW2Box12Code}
\alias{getFederalW2Box12Code}
\title{Get a FederalW2Box12Code}
\usage{
getFederalW2Box12Code(
  FederalW2Box12CodeID,
  Code = F,
  CodeDescription = F,
  CreatedTime = F,
  Description = F,
  IsInvalid = F,
  ModifiedTime = F,
  SkywardHash = F,
  SkywardID = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  ValidYearHigh = F,
  ValidYearLow = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{FederalW2Box12CodeID}{The ID of the FederalW2Box12Code to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FederalW2Box12Code') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given FederalW2Box12Code. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FederalW2Box12Code.}
}
\value{
A dataframe or of FederalW2Box12Code
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a FederalW2Box12Code
}
\concept{StateReportingFederal}
