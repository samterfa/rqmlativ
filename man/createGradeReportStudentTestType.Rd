% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Grading.R
\name{createGradeReportStudentTestType}
\alias{createGradeReportStudentTestType}
\title{Create a GradeReportStudentTestType}
\usage{
createGradeReportStudentTestType(
  GradeReportStudentID = NULL,
  SortNumber = NULL,
  TestColumnHeader1 = NULL,
  TestColumnHeader10 = NULL,
  TestColumnHeader2 = NULL,
  TestColumnHeader3 = NULL,
  TestColumnHeader4 = NULL,
  TestColumnHeader5 = NULL,
  TestColumnHeader6 = NULL,
  TestColumnHeader7 = NULL,
  TestColumnHeader8 = NULL,
  TestColumnHeader9 = NULL,
  TestID = NULL,
  TestName = NULL,
  TestVersionID = NULL,
  TestVersionName = NULL,
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

\item{fieldNames}{The field values to give the created GradeReportStudentTestType. Each defaults to NULL.}
}
\value{
A newly created GradeReportStudentTestType
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a GradeReportStudentTestType
}
\concept{Grading}
