% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Scheduling.R
\name{getTempCourse}
\alias{getTempCourse}
\title{Get a TempCourse}
\usage{
getTempCourse(
  TempCourseID,
  ActiveSections = F,
  AveragePerSectionMinimumSectionsRequired = F,
  CodeDescription = F,
  CourseCode = F,
  CourseID = F,
  CourseLengthCode = F,
  CourseLengthID = F,
  CourseSubjectCode = F,
  CourseTypeCode = F,
  CreatedTime = F,
  CurriculumCode = F,
  DefaultSectionLengthID = F,
  Description = F,
  EarnedCredits = F,
  EntityCode = F,
  EstimatedNumberOfSections = F,
  EstimatedStudentsPerSection = F,
  GradeLevelSummary = F,
  GradingPeriodSetCode = F,
  GradingPeriodSetID = F,
  IsActive = F,
  MinimumSectionsRequired = F,
  ModifiedTime = F,
  NewCourseLengthCode = F,
  NewCourseLengthID = F,
  NewGradingPeriodSetCode = F,
  NewGradingPeriodSetID = F,
  Note = F,
  NumberOfAlternateCourseRequests = F,
  NumberOfCourseRequests = F,
  NumberOfSeatsAvailable = F,
  NumberOfTransferStudentSections = F,
  ObjectIsDirty = F,
  ObjectName = F,
  OriginalEstimatedNumberOfSections = F,
  RecordsUpdated = F,
  RowIsReadOnly = F,
  RowIsSelected = F,
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
\item{TempCourseID}{The ID of the TempCourse to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCourse') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given TempCourse. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCourse.}
}
\value{
A dataframe or of TempCourse
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a TempCourse
}
\concept{Scheduling}
