% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gradebook.R
\name{createCalculationGroup}
\alias{createCalculationGroup}
\title{Create a CalculationGroup}
\usage{
createCalculationGroup(
  AllowAcademicStandards = NULL,
  AllowAcademicStandardWeightingTeacherOverride = NULL,
  AllowAssignmentAveragePercentTeacherOverride = NULL,
  AllowAssignmentPointsTeacherOverride = NULL,
  AllowCategoryOverride = NULL,
  AllowCategoryWeightingTeacherOverride = NULL,
  AllowDecayingAverageTeacherOverride = NULL,
  AllowGradeBucketWeightingTeacherOverride = NULL,
  AllowNotGradedTeacherOverride = NULL,
  AllowSubjectiveTeacherOverride = NULL,
  AllowSubjects = NULL,
  AllowSubjectWeightingTeacherOverride = NULL,
  AllowTotalPointsAndGradeBucketWeightingTeacherOverride = NULL,
  CalculationGroupIDClonedFrom = NULL,
  DecayingAveragePercent = NULL,
  Description = NULL,
  EntityGroupKey = NULL,
  EntityID = NULL,
  IsDefaultGroup = NULL,
  LimitTeacherOverridesToCategoriesInGroup = NULL,
  Rank = NULL,
  SchoolYearID = NULL,
  Status = NULL,
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

\item{fieldNames}{The field values to give the created CalculationGroup. Each defaults to NULL.}
}
\value{
A newly created CalculationGroup
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a CalculationGroup
}
\concept{Gradebook}
