% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SkySys.R
\name{modifyApplySchemaChangeFieldSelection}
\alias{modifyApplySchemaChangeFieldSelection}
\title{Modify an ApplySchemaChangeFieldSelection}
\usage{
modifyApplySchemaChangeFieldSelection(
  ApplySchemaChangeFieldSelectionID,
  FieldChangeType = NULL,
  FieldCurrentName = NULL,
  FieldHasChangedRelationships = NULL,
  FieldPendingName = NULL,
  ModulePendingName = NULL,
  ObjectPendingName = NULL,
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

\item{fieldNames}{The field values to give the modified ApplySchemaChangeFieldSelection. Each defaults to NULL.}
}
\value{
The modified ApplySchemaChangeFieldSelection
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies an ApplySchemaChangeFieldSelection
}
\concept{SkySys}
