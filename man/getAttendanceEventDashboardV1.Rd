% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReporting.R
\name{getAttendanceEventDashboardV1}
\alias{getAttendanceEventDashboardV1}
\title{Get an AttendanceEventDashboardV1}
\usage{
getAttendanceEventDashboardV1(
  AttendanceEventDashboardV1ID,
  CategoryCode = F,
  ClassPeriodName = F,
  CreatedTime = F,
  DashboardRunHistoryID = F,
  Date = F,
  EdFiAttendanceEventCategoryID = F,
  EdFiAttendanceEventTypeID = F,
  EdFiEducationalEnvironmentID = F,
  EdFiTermTypeID = F,
  EducationalEnvironmentCode = F,
  EntityID = F,
  KeyHash = F,
  LocalCourseCode = F,
  Location = F,
  ModifiedTime = F,
  Reason = F,
  SchoolCode = F,
  SchoolYearDescription = F,
  SchoolYearID = F,
  StudentID = F,
  StudentUniqueID = F,
  TermCode = F,
  TypeCode = F,
  UpdateHash = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{AttendanceEventDashboardV1ID}{The ID of the AttendanceEventDashboardV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceEventDashboardV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given AttendanceEventDashboardV1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceEventDashboardV1.}
}
\value{
A dataframe or of AttendanceEventDashboardV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of an AttendanceEventDashboardV1
}
\concept{StateReporting}
