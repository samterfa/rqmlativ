% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OneRoster.R
\name{getLineItem}
\alias{getLineItem}
\title{Get a LineItem}
\usage{
getLineItem(
  LineItemID,
  AssignDate = F,
  AssignmentID = F,
  CategorySourcedID = F,
  ClassSourcedID = F,
  DateLastModified = F,
  Description = F,
  DueDate = F,
  GradingPeriodSourcedID = F,
  ModifiedTime = F,
  OneRosterID = F,
  ResultValueMax = F,
  ResultValueMin = F,
  SectionID = F,
  SourcedID = F,
  Status = F,
  Title = F,
  VendorSourceID = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{LineItemID}{The ID of the LineItem to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LineItem') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given LineItem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LineItem.}
}
\value{
A dataframe or of LineItem
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a LineItem
}
\concept{OneRoster}
