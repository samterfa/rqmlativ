% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SkySys.R
\name{getOnlinePaymentVendorEntity}
\alias{getOnlinePaymentVendorEntity}
\title{Get an OnlinePaymentVendorEntity}
\usage{
getOnlinePaymentVendorEntity(
  OnlinePaymentVendorEntityID,
  CreatedTime = F,
  EntityID = F,
  ModifiedTime = F,
  OnlinePaymentVendorID = F,
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
\item{OnlinePaymentVendorEntityID}{The ID of the OnlinePaymentVendorEntity to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlinePaymentVendorEntity') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given OnlinePaymentVendorEntity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlinePaymentVendorEntity.}
}
\value{
A dataframe or of OnlinePaymentVendorEntity
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of an OnlinePaymentVendorEntity
}
\concept{SkySys}
