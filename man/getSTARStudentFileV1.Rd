% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{getSTARStudentFileV1}
\alias{getSTARStudentFileV1}
\title{Get a STARStudentFileV1}
\usage{
getSTARStudentFileV1(
  STARStudentFileV1ID,
  EntityID = F,
  SchoolYearID = F,
  CourseID = F,
  SectionID = F,
  StaffID = F,
  SchoolID = F,
  STARStudentSubmissionID = F,
  STARStudentSubmissionRunHistoryID = F,
  CurriculumYearID = F,
  StateDistrictMNID = F,
  DistrictNumber = F,
  StateDistrictTypeCodeMNID = F,
  DistrictType = F,
  FileFolderNumber = F,
  SchoolNumber = F,
  StateSTARAssignmentCodeMNID = F,
  AssignmentCode = F,
  StateSTARGradeLevelMNID = F,
  GradeLevel = F,
  StateSTARModeOfTeachingMNID = F,
  TeachingMode = F,
  PeriodsPerWeek = F,
  LengthOfPeriod = F,
  NumberOfPupils = F,
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
\item{STARStudentFileV1ID}{The ID of the STARStudentFileV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('STARStudentFileV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given STARStudentFileV1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the STARStudentFileV1.}
}
\value{
A dataframe or of STARStudentFileV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a STARStudentFileV1
}
\concept{StateReportingMN}