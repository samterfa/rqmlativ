% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gradebook.R
\name{modifyStudentAssignment}
\alias{modifyStudentAssignment}
\title{Modify a StudentAssignment}
\usage{
modifyStudentAssignment(
  StudentAssignmentID,
  AssignmentID = NULL,
  CalculatedPoints = NULL,
  Comments = NULL,
  GradeMarkID = NULL,
  IsDropped = NULL,
  IsGraded = NULL,
  IsMissing = NULL,
  NoCount = NULL,
  OnlineAssignmentNotificationSent = NULL,
  OnlineAssignmentReviewNotificationSent = NULL,
  Score = NULL,
  ScoreClarifierID = NULL,
  SectionID = NULL,
  StudentSectionID = NULL,
  SubmissionStatus = NULL,
  SubmissionTime = NULL,
  TimeLastScored = NULL,
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

\item{fieldNames}{The field values to give the modified StudentAssignment. Each defaults to NULL.}
}
\value{
The modified StudentAssignment
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a StudentAssignment
}
\concept{Gradebook}
