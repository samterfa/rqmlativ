% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/YearEnd.R
\name{modifyTempRollUnpaidCustomerFee}
\alias{modifyTempRollUnpaidCustomerFee}
\title{Modify a TempRollUnpaidCustomerFee}
\usage{
modifyTempRollUnpaidCustomerFee(
  TempRollUnpaidCustomerFeeID,
  AmountDue = NULL,
  Comment = NULL,
  CreateCustomerEntityYear = NULL,
  CustomerID = NULL,
  CustomerName = NULL,
  Description = NULL,
  DueDate = NULL,
  EffectiveDate = NULL,
  ExceptionReason = NULL,
  FeeIDTransferFrom = NULL,
  RollType = NULL,
  SourceCustomerFeeID = NULL,
  SourceFeeAmount = NULL,
  SourceFeeCode = NULL,
  SourceFeeID = NULL,
  TargetEntityCode = NULL,
  TargetEntityID = NULL,
  UnappliedAmount = NULL,
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

\item{fieldNames}{The field values to give the modified TempRollUnpaidCustomerFee. Each defaults to NULL.}
}
\value{
The modified TempRollUnpaidCustomerFee
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a TempRollUnpaidCustomerFee
}
\concept{YearEnd}
