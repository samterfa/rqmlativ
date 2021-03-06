% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Grading.R
\name{modifyGradeReportStudentTestRow}
\alias{modifyGradeReportStudentTestRow}
\title{Modify a GradeReportStudentTestRow}
\usage{
modifyGradeReportStudentTestRow(
  GradeReportStudentTestRowID,
  DateTaken = NULL,
  GradeReportStudentTestTypeID = NULL,
  SortNumber = NULL,
  TestColumn1 = NULL,
  TestColumn10 = NULL,
  TestColumn2 = NULL,
  TestColumn3 = NULL,
  TestColumn4 = NULL,
  TestColumn5 = NULL,
  TestColumn6 = NULL,
  TestColumn7 = NULL,
  TestColumn8 = NULL,
  TestColumn9 = NULL,
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

\item{fieldNames}{The field values to give the modified GradeReportStudentTestRow. Each defaults to NULL.}
}
\value{
The modified GradeReportStudentTestRow
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a GradeReportStudentTestRow
}
\concept{Grading}
