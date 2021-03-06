% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Grading.R
\name{modifyTempStudentGradeBucketFlag}
\alias{modifyTempStudentGradeBucketFlag}
\title{Modify a TempStudentGradeBucketFlag}
\usage{
modifyTempStudentGradeBucketFlag(
  TempStudentGradeBucketFlagID,
  CourseSectionCode = NULL,
  ExceptionReason = NULL,
  GradeBucketFlagCode = NULL,
  GradeBucketFlagID = NULL,
  GradeBucketLabel = NULL,
  GradeMarkCode = NULL,
  IsDelete = NULL,
  IsError = NULL,
  IsException = NULL,
  IsManual = NULL,
  SchoolYearDescription = NULL,
  SectionLengthCode = NULL,
  SelectedToProcess = NULL,
  StudentGradeBucketFlagIDToDelete = NULL,
  StudentGradeBucketID = NULL,
  StudentName = NULL,
  StudentNumber = NULL,
  StudentSectionDescription = NULL,
  StudentSectionID = NULL,
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

\item{fieldNames}{The field values to give the modified TempStudentGradeBucketFlag. Each defaults to NULL.}
}
\value{
The modified TempStudentGradeBucketFlag
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a TempStudentGradeBucketFlag
}
\concept{Grading}
