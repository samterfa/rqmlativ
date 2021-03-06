% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Scheduling.R
\name{createStudentAutoSchedulerRunAnalysisTotal}
\alias{createStudentAutoSchedulerRunAnalysisTotal}
\title{Create a StudentAutoSchedulerRunAnalysisTotal}
\usage{
createStudentAutoSchedulerRunAnalysisTotal(
  ConflictPercent = NULL,
  EndTimeAnalysis = NULL,
  GradeReferenceID = NULL,
  StartTimeAnalysis = NULL,
  StudentAutoSchedulerRunAnalysisID = NULL,
  StudentCourseRequestsConflictedThisRun = NULL,
  StudentCourseRequestsProcessedThisRun = NULL,
  StudentCourseRequestsScheduledThisRun = NULL,
  StudentsWithConflicts = NULL,
  StudentsWithOneConflict = NULL,
  StudentsWithThreeOrMoreConflicts = NULL,
  StudentsWithTwoConflicts = NULL,
  TotalAlternateStudentCourseRequests = NULL,
  TotalConflictedStudentCourseRequests = NULL,
  TotalScheduledStudentCourseRequests = NULL,
  TotalStudentCourseRequests = NULL,
  TotalStudents = NULL,
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

\item{fieldNames}{The field values to give the created StudentAutoSchedulerRunAnalysisTotal. Each defaults to NULL.}
}
\value{
A newly created StudentAutoSchedulerRunAnalysisTotal
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a StudentAutoSchedulerRunAnalysisTotal
}
\concept{Scheduling}
