% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FoodService.R
\name{getApplicationSectionElement}
\alias{getApplicationSectionElement}
\title{Get an ApplicationSectionElement}
\usage{
getApplicationSectionElement(
  ApplicationSectionElementID,
  ApplicationSectionID = F,
  ApplicationSectionElementGroupID = F,
  ApplicationSectionDependentElementGroupID = F,
  FieldPathGuid = F,
  DisplayNameOverride = F,
  DisplayFormatOverride = F,
  LineNumber = F,
  DisplayOrder = F,
  IsHidden = F,
  IsReadOnly = F,
  DefaultValue = F,
  ElementViewPart = F,
  UserIDCreatedBy = F,
  CreatedTime = F,
  UserIDModifiedBy = F,
  ModifiedTime = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{ApplicationSectionElementID}{The ID of the ApplicationSectionElement to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationSectionElement') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given ApplicationSectionElement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationSectionElement.}
}
\value{
A dataframe or of ApplicationSectionElement
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of an ApplicationSectionElement
}
\concept{FoodService}