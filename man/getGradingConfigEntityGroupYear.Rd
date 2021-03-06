% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Grading.R
\name{getGradingConfigEntityGroupYear}
\alias{getGradingConfigEntityGroupYear}
\title{Get a GradingConfigEntityGroupYear}
\usage{
getGradingConfigEntityGroupYear(
  GradingConfigEntityGroupYearID,
  ConfigEntityGroupYearID = F,
  ConfigEntityGroupYearIDClonedFrom = F,
  CreatedTime = F,
  CurrentCalculation = F,
  EarnedCreditsMethodIDDefault = F,
  EntityGroupKey = F,
  EntityID = F,
  FreeFormCommentMaxLength = F,
  GradebookLockMessage = F,
  GradeLevelIDCohort = F,
  LockGradebookAssignmentsAfterDate = F,
  LockGradebookCalculation = F,
  LockGradebookStartTime = F,
  LockGradeBuckets = F,
  ModifiedTime = F,
  RetainGradesNumberOfDays = F,
  SchoolYearID = F,
  UseAddOnGPA = F,
  UseFactorBasedAddOn = F,
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
\item{GradingConfigEntityGroupYearID}{The ID of the GradingConfigEntityGroupYear to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GradingConfigEntityGroupYear') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given GradingConfigEntityGroupYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GradingConfigEntityGroupYear.}
}
\value{
A dataframe or of GradingConfigEntityGroupYear
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a GradingConfigEntityGroupYear
}
\concept{Grading}
