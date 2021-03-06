% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{createMCCCStaffV1}
\alias{createMCCCStaffV1}
\title{Create a MCCCStaffV1}
\usage{
createMCCCStaffV1(
  AcademicYearNumber = NULL,
  CalendarNumber = NULL,
  CourseID = NULL,
  EntityID = NULL,
  FileFolderNumber = NULL,
  InstructionLanguage = NULL,
  InstructionMethod = NULL,
  InstructionMinutes = NULL,
  IsFixedPeriod = NULL,
  IsTeacherOfRecord = NULL,
  LocalCourseCode = NULL,
  MarkingIndicator = NULL,
  MCCCAcademicYearImportID = NULL,
  MCCCCalendarImportID = NULL,
  MCCCSubmissionID = NULL,
  MCCCSubmissionRunHistoryID = NULL,
  MCCCTermImportID = NULL,
  MeetID = NULL,
  PeriodData = NULL,
  SchoolID = NULL,
  SchoolNumber = NULL,
  SchoolYearID = NULL,
  SectionCode = NULL,
  SectionID = NULL,
  StaffID = NULL,
  StaffMeetID = NULL,
  StateInstructionalMethodCodeMNID = NULL,
  StateLanguageCodeMNID = NULL,
  TermNumber = NULL,
  TermType = NULL,
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

\item{fieldNames}{The field values to give the created MCCCStaffV1. Each defaults to NULL.}
}
\value{
A newly created MCCCStaffV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a MCCCStaffV1
}
\concept{StateReportingMN}
