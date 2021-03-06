% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReporting.R
\name{createStudentGradeDashboardV1}
\alias{createStudentGradeDashboardV1}
\title{Create a StudentGradeDashboardV1}
\usage{
createStudentGradeDashboardV1(
  ClassPeriodName = NULL,
  DashboardRunHistoryID = NULL,
  DiagnosticStatement = NULL,
  EdFiGradeTypeID = NULL,
  EdFiGradingPeriodID = NULL,
  EdFiPerformanceBaseTypeID = NULL,
  EdFiTermTypeID = NULL,
  EntityID = NULL,
  GradeTypeCode = NULL,
  GradingPeriodCode = NULL,
  LetterGradeEarned = NULL,
  LocalCourseCode = NULL,
  Location = NULL,
  NumericGradeEarned = NULL,
  PerformanceBaseCode = NULL,
  SchoolCode = NULL,
  SchoolYearDescription = NULL,
  SchoolYearID = NULL,
  StudentID = NULL,
  StudentUniqueID = NULL,
  TermCode = NULL,
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

\item{fieldNames}{The field values to give the created StudentGradeDashboardV1. Each defaults to NULL.}
}
\value{
A newly created StudentGradeDashboardV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a StudentGradeDashboardV1
}
\concept{StateReporting}
