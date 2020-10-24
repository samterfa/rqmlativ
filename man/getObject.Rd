% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SkySys.R
\name{getObject}
\alias{getObject}
\title{Get an Object}
\usage{
getObject(
  ObjectID,
  PendingName = F,
  CurrentName = F,
  SkywardID = F,
  SkywardHash = F,
  PendingDisplayName = F,
  CurrentDisplayName = F,
  ModuleID = F,
  FormattedObjectPath = F,
  PendingFormattedObjectPath = F,
  IsSkywardObject = F,
  Status = F,
  IsInDB = F,
  IsTempDataObject = F,
  AllowAttachments = F,
  HasChangedFields = F,
  HasChangedRelationships = F,
  UserIDCreatedBy = F,
  CreatedTime = F,
  UserIDModifiedBy = F,
  ModifiedTime = F,
  Scope = F,
  CustomizationID = F,
  UniqueID = F,
  CodeGuidFieldPath = F,
  DescriptionGuidFieldPath = F,
  EffectiveName = F,
  EffectiveDisplayName = F,
  HasDefaultSortGroup = F,
  ViewSQLText = F,
  IsView = F,
  NotForDisplayInReporting = F,
  NotForDisplayInImporting = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{ObjectID}{The ID of the Object to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Object') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given Object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Object.}
}
\value{
A dataframe or of Object
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of an Object
}
\concept{SkySys}