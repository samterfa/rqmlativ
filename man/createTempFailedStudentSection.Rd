% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Scheduling.R
\name{createTempFailedStudentSection}
\alias{createTempFailedStudentSection}
\title{Create a TempFailedStudentSection}
\usage{
createTempFailedStudentSection(
  AutomaticRequestSetting = NULL,
  AutomaticScheduleSetting = NULL,
  CourseCode = NULL,
  CourseCodeDescription = NULL,
  CourseDescription = NULL,
  CourseEntityOfferedToID = NULL,
  CourseGradeLevelSummary = NULL,
  CourseID = NULL,
  EarlyExitReasonCodeDescription = NULL,
  EndDate = NULL,
  EntityIDCountsAs = NULL,
  EntityIDCourse = NULL,
  GradeReferenceID = NULL,
  Note = NULL,
  RenderCheckbox = NULL,
  RowIsReadOnly = NULL,
  RowIsSelected = NULL,
  ScheduleAllSectionsInGroupOrNone = NULL,
  SchoolYearIDCourse = NULL,
  SectionCode = NULL,
  SectionCorequisiteGroupName = NULL,
  SectionID = NULL,
  SectionLengthCode = NULL,
  SectionLengthEndDate = NULL,
  SectionLengthID = NULL,
  SectionLengthStartDate = NULL,
  SectionLengthSubsetID = NULL,
  StartDate = NULL,
  StudentCourseRequestID = NULL,
  StudentGenderCode = NULL,
  StudentGradeLevelCode = NULL,
  StudentGradYear = NULL,
  StudentID = NULL,
  StudentNameLFM = NULL,
  StudentNumber = NULL,
  StudentSectionID = NULL,
  StudentSectionTransactionIDToUpdate = NULL,
  TempStudentID = NULL,
  TempStudentSectionID = NULL,
  WorkflowAction = NULL,
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

\item{fieldNames}{The field values to give the created TempFailedStudentSection. Each defaults to NULL.}
}
\value{
A newly created TempFailedStudentSection
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a TempFailedStudentSection
}
\concept{Scheduling}
