% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FoodService.R
\name{getLineCloseout}
\alias{getLineCloseout}
\title{Get a LineCloseout}
\usage{
getLineCloseout(
  LineCloseoutID,
  CashReceivedAmount = F,
  CheckReceivedAmount = F,
  CloseoutTime = F,
  CreatedTime = F,
  FinalizedByFullNameLFM = F,
  IsFinalized = F,
  LatestCloseoutAmount = F,
  LatestStartingBalance = F,
  LineID = F,
  ModifiedTime = F,
  StartTime = F,
  TotalReceivedAmount = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{LineCloseoutID}{The ID of the LineCloseout to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LineCloseout') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given LineCloseout. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LineCloseout.}
}
\value{
A dataframe or of LineCloseout
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a LineCloseout
}
\concept{FoodService}
