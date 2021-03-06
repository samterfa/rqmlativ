% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Attendance.R
\name{createStudentAttendance}
\alias{createStudentAttendance}
\title{Create a StudentAttendance}
\usage{
createStudentAttendance(
  CalendarDayID = NULL,
  Comment = NULL,
  DaysAbsent = NULL,
  DaysExcused = NULL,
  DaysOther = NULL,
  DaysUnexcused = NULL,
  EntityID = NULL,
  HideRecordMA = NULL,
  IsGuardianNotified = NULL,
  SchoolYearID = NULL,
  StudentID = NULL,
  TardyCount = NULL,
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

\item{fieldNames}{The field values to give the created StudentAttendance. Each defaults to NULL.}
}
\value{
A newly created StudentAttendance
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a StudentAttendance
}
\concept{Attendance}
