% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FoodService.R
\name{createDistrictSchoolReimbursementMonthlyClaim}
\alias{createDistrictSchoolReimbursementMonthlyClaim}
\title{Create a DistrictSchoolReimbursementMonthlyClaim}
\usage{
createDistrictSchoolReimbursementMonthlyClaim(
  ApprovedCountFree = NULL,
  ApprovedCountReduced = NULL,
  DistrictID = NULL,
  MealType = NULL,
  NumericDay = NULL,
  PaidEligible = NULL,
  ReimbursementMonthID = NULL,
  ServedCountCEP = NULL,
  ServedCountFree = NULL,
  ServedCountOther = NULL,
  ServedCountPaid = NULL,
  ServedCountReduced = NULL,
  ServedCountTotal = NULL,
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

\item{fieldNames}{The field values to give the created DistrictSchoolReimbursementMonthlyClaim. Each defaults to NULL.}
}
\value{
A newly created DistrictSchoolReimbursementMonthlyClaim
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a DistrictSchoolReimbursementMonthlyClaim
}
\concept{FoodService}
