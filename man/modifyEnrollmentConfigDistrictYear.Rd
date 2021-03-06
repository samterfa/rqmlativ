% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Enrollment.R
\name{modifyEnrollmentConfigDistrictYear}
\alias{modifyEnrollmentConfigDistrictYear}
\title{Modify an EnrollmentConfigDistrictYear}
\usage{
modifyEnrollmentConfigDistrictYear(
  ConfigDistrictYearID,
  AutoAddSchoolPathOverride = NULL,
  ConfigDistrictYearIDClonedFrom = NULL,
  DefaultRetainInterventionPlanRecords = NULL,
  DistrictID = NULL,
  EnableNoShow = NULL,
  EnrolledDifferentEntityNoShowActionType = NULL,
  EnrolledDifferentEntityNoShowEntryDate = NULL,
  EnrolledDifferentEntityNoShowWithdrawalDate = NULL,
  NoDistrictEnrollmentNoShowActionType = NULL,
  NoDistrictEnrollmentNoShowEntryDate = NULL,
  NoDistrictEnrollmentNoShowWithdrawalDate = NULL,
  PermitIDAutoAdd = NULL,
  PreviouslyEnrolledSameEntityNoShowActionType = NULL,
  PreviouslyEnrolledSameEntityNoShowEntryDate = NULL,
  PreviouslyEnrolledSameEntityNoShowWithdrawalDate = NULL,
  PriorNoShowRecord = NULL,
  SchoolYearID = NULL,
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

\item{fieldNames}{The field values to give the modified EnrollmentConfigDistrictYear. Each defaults to NULL.}
}
\value{
The modified EnrollmentConfigDistrictYear
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies an EnrollmentConfigDistrictYear
}
\concept{Enrollment}
