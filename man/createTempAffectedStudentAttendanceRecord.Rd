% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Attendance.R
\name{createTempAffectedStudentAttendanceRecord}
\alias{createTempAffectedStudentAttendanceRecord}
\title{Create a TempAffectedStudentAttendanceRecord}
\usage{
createTempAffectedStudentAttendanceRecord(
  AffectedPrimaryKey = NULL,
  CalendarDayID = NULL,
  Comment = NULL,
  Date = NULL,
  DayRotationCode = NULL,
  FailedStudentAttendancePeriods = NULL,
  FailureReason = NULL,
  FullName = NULL,
  IsGuardianNotified = NULL,
  NewDaysAbsent = NULL,
  NewDaysExcused = NULL,
  NewDaysOther = NULL,
  NewDaysUnexcused = NULL,
  NewGuardianNotified = NULL,
  NewStudentAttendancePeriods = NULL,
  NewTardyCount = NULL,
  OldDaysAbsent = NULL,
  OldDaysExcused = NULL,
  OldDaysOther = NULL,
  OldDaysUnexcused = NULL,
  OldStudentAttendancePeriods = NULL,
  OldTardyCount = NULL,
  PreviousGuardianNotified = NULL,
  StudentID = NULL,
  StudentNumber = NULL,
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

\item{fieldNames}{The field values to give the created TempAffectedStudentAttendanceRecord. Each defaults to NULL.}
}
\value{
A newly created TempAffectedStudentAttendanceRecord
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a TempAffectedStudentAttendanceRecord
}
\concept{Attendance}
