% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MessageCenter.R
\name{getNotificationWithdrawalCode}
\alias{getNotificationWithdrawalCode}
\title{Get a NotificationWithdrawalCode}
\usage{
getNotificationWithdrawalCode(
  NotificationWithdrawalCodeID,
  CreatedTime = F,
  ModifiedTime = F,
  NotificationID = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  WithdrawalCodeID = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{NotificationWithdrawalCodeID}{The ID of the NotificationWithdrawalCode to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationWithdrawalCode') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given NotificationWithdrawalCode. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationWithdrawalCode.}
}
\value{
A dataframe or of NotificationWithdrawalCode
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a NotificationWithdrawalCode
}
\concept{MessageCenter}
