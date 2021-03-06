% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{modifyForm1099NECEmployerException}
\alias{modifyForm1099NECEmployerException}
\title{Modify a Form1099NECEmployerException}
\usage{
modifyForm1099NECEmployerException(
  Form1099NECEmployerExceptionID,
  Form1099NECEmployerV1ID = NULL,
  Form1099NECSubmissionID = NULL,
  Form1099NECSubmissionRunHistoryID = NULL,
  Message = NULL,
  MessageType = NULL,
  ProcessType = NULL,
  RuleNumber = NULL,
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

\item{fieldNames}{The field values to give the modified Form1099NECEmployerException. Each defaults to NULL.}
}
\value{
The modified Form1099NECEmployerException
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a Form1099NECEmployerException
}
\concept{StateReportingFederal}
