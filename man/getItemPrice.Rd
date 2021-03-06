% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FoodService.R
\name{getItemPrice}
\alias{getItemPrice}
\title{Get an ItemPrice}
\usage{
getItemPrice(
  ItemPriceID,
  AdditionalPurchaseAmount = F,
  CreatedTime = F,
  DistrictGroupKey = F,
  EffectiveDate = F,
  EffectiveTime = F,
  EligibilityCategoryID = F,
  IsCurrentActivePrice = F,
  IsGovernmentFunded = F,
  ItemID = F,
  ItemLimit = F,
  ModifiedTime = F,
  NeedsAdditionalSetup = F,
  PriceCategoryID = F,
  PurchaseAmount = F,
  RenderIsGovernmentFundedReadOnly = F,
  StopAdditionalPurchases = F,
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
\item{ItemPriceID}{The ID of the ItemPrice to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ItemPrice') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given ItemPrice. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ItemPrice.}
}
\value{
A dataframe or of ItemPrice
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of an ItemPrice
}
\concept{FoodService}
