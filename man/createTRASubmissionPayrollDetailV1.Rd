% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{createTRASubmissionPayrollDetailV1}
\alias{createTRASubmissionPayrollDetailV1}
\title{Create a TRASubmissionPayrollDetailV1}
\usage{
createTRASubmissionPayrollDetailV1(
  DistrictID = NULL,
  EmployeeDeductionAmount = NULL,
  EmployeeID = NULL,
  EmployeeNumber = NULL,
  EmployerDeductionAmount = NULL,
  PaymentDate = NULL,
  PayPeriodBeginDate = NULL,
  PayPeriodEndDate = NULL,
  Salary = NULL,
  SocialSecurityNumber = NULL,
  StateRetirementAssociationTypeCode = NULL,
  StateTRACurrentPositionMNID = NULL,
  StateTRACurrentPositionMNValue = NULL,
  StateTRAExemptStatusMNCode = NULL,
  StateTRAExemptStatusMNID = NULL,
  StateTransactionTypeMNID = NULL,
  StateTransactionTypeMNValue = NULL,
  StateTRAPaymentTypeMNID = NULL,
  StateTRAPaymentTypeMNValue = NULL,
  StateTRAPayrollFrequencyMNID = NULL,
  StateTRAPayrollFrequencyMNValue = NULL,
  TRAFiscalYear = NULL,
  TRANumber = NULL,
  TRASubmissionID = NULL,
  TRASubmissionRunHistoryID = NULL,
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

\item{fieldNames}{The field values to give the created TRASubmissionPayrollDetailV1. Each defaults to NULL.}
}
\value{
A newly created TRASubmissionPayrollDetailV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a TRASubmissionPayrollDetailV1
}
\concept{StateReportingMN}
