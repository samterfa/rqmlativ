% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Grading.R
\name{modifyGradeReportAcademicSession}
\alias{modifyGradeReportAcademicSession}
\title{Modify a GradeReportAcademicSession}
\usage{
modifyGradeReportAcademicSession(
  GradeReportAcademicSessionID,
  DaysAbsentYTD = NULL,
  DaysEnrolledYTD = NULL,
  DaysExcusedYTD = NULL,
  DaysUnexcusedYTD = NULL,
  EarnedCredit = NULL,
  EarnedCreditAttempted = NULL,
  EarnedCreditsValue = NULL,
  EntryDate = NULL,
  GPAValue = NULL,
  GradeLevelCode = NULL,
  GradeReportAcademicSessionTemplateGroupID = NULL,
  GradeReportStudentID = NULL,
  HeaderDescription = NULL,
  SchoolYearDescription = NULL,
  SchoolYearID = NULL,
  SortNumber = NULL,
  WithdrawalDate = NULL,
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

\item{fieldNames}{The field values to give the modified GradeReportAcademicSession. Each defaults to NULL.}
}
\value{
The modified GradeReportAcademicSession
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a GradeReportAcademicSession
}
\concept{Grading}
