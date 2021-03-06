% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{modifyMCCCStudentCourseV1}
\alias{modifyMCCCStudentCourseV1}
\title{Modify a MCCCStudentCourseV1}
\usage{
modifyMCCCStudentCourseV1(
  MCCCStudentCourseV1ID,
  AcademicYearNumber = NULL,
  BirthDate = NULL,
  CalendarNumber = NULL,
  CollegeCode = NULL,
  CollegeCourseCode = NULL,
  CollegeCourseTitle = NULL,
  CollegeCredits = NULL,
  CollegeMarkEarned = NULL,
  CourseID = NULL,
  CourseLevelMNID = NULL,
  CurriculumYearID = NULL,
  EarlyEducationInstructionalMinutes = NULL,
  EntityID = NULL,
  FileFolderNumber = NULL,
  FirstName = NULL,
  Gender = NULL,
  GradeMark = NULL,
  GradeMarkIDEarned = NULL,
  InstitutionID = NULL,
  LastName = NULL,
  LocalCourseCode = NULL,
  LocalCreditsEarned = NULL,
  MarkingTermNumber = NULL,
  MarkingTermType = NULL,
  MCCCAcademicYearImportID = NULL,
  MCCCCalendarImportID = NULL,
  MCCCSubmissionID = NULL,
  MCCCSubmissionRunHistoryID = NULL,
  MCCCTermImportID = NULL,
  MCCCTermImportIDMarkingTerm = NULL,
  MeetID = NULL,
  MiddleName = NULL,
  RecordType = NULL,
  SchoolID = NULL,
  SchoolNumber = NULL,
  SchoolYearID = NULL,
  SectionCode = NULL,
  SectionID = NULL,
  StaffID = NULL,
  StateCourseCode = NULL,
  StateCourseCodeMNID = NULL,
  StateGradeMarkMNID = NULL,
  StateSubjectAreaCodeMNID = NULL,
  StudentGradeBucketID = NULL,
  StudentID = NULL,
  StudentStateID = NULL,
  SubjectArea = NULL,
  Suffix = NULL,
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

\item{fieldNames}{The field values to give the modified MCCCStudentCourseV1. Each defaults to NULL.}
}
\value{
The modified MCCCStudentCourseV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a MCCCStudentCourseV1
}
\concept{StateReportingMN}
