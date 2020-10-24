% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{getMCCCStaffV1}
\alias{getMCCCStaffV1}
\title{Get a MCCCStaffV1}
\usage{
getMCCCStaffV1(
  MCCCStaffV1ID,
  LocalCourseCode = F,
  SectionCode = F,
  SchoolNumber = F,
  MCCCAcademicYearImportID = F,
  AcademicYearNumber = F,
  MCCCCalendarImportID = F,
  CalendarNumber = F,
  MCCCTermImportID = F,
  TermType = F,
  TermNumber = F,
  FileFolderNumber = F,
  IsFixedPeriod = F,
  MarkingIndicator = F,
  StateInstructionalMethodCodeMNID = F,
  InstructionMethod = F,
  StateLanguageCodeMNID = F,
  InstructionLanguage = F,
  InstructionMinutes = F,
  IsTeacherOfRecord = F,
  SchoolID = F,
  EntityID = F,
  SchoolYearID = F,
  CourseID = F,
  SectionID = F,
  MeetID = F,
  StaffMeetID = F,
  StaffID = F,
  MCCCSubmissionID = F,
  MCCCSubmissionRunHistoryID = F,
  PeriodData = F,
  PeriodCodes = F,
  HasErrors = F,
  UserIDCreatedBy = F,
  CreatedTime = F,
  UserIDModifiedBy = F,
  ModifiedTime = F,
  KeyHash = F,
  UpdateHash = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{MCCCStaffV1ID}{The ID of the MCCCStaffV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MCCCStaffV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given MCCCStaffV1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MCCCStaffV1.}
}
\value{
A dataframe or of MCCCStaffV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a MCCCStaffV1
}
\concept{StateReportingMN}