% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Attendance.R
\name{modifyStudentDisciplineThresholdAttendanceReportRunHistory}
\alias{modifyStudentDisciplineThresholdAttendanceReportRunHistory}
\title{Modify a StudentDisciplineThresholdAttendanceReportRunHistory}
\usage{
modifyStudentDisciplineThresholdAttendanceReportRunHistory(
  StudentDisciplineThresholdAttendanceReportRunHistoryID,
  StudentID = NULL,
  AttendanceReportRunHistoryID = NULL,
  DisciplineThresholdID = NULL,
  IsActive = NULL,
  AttachmentID = NULL,
  Header = NULL,
  Body = NULL,
  Footer = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{The field values to give the modified StudentDisciplineThresholdAttendanceReportRunHistory. Each defaults to NULL.}
}
\value{
The modified StudentDisciplineThresholdAttendanceReportRunHistory
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a StudentDisciplineThresholdAttendanceReportRunHistory
}
\concept{Attendance}