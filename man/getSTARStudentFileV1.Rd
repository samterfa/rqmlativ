% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{getSTARStudentFileV1}
\alias{getSTARStudentFileV1}
\title{Get a STARStudentFileV1}
\usage{
getSTARStudentFileV1(
  STARStudentFileV1ID,
  AssignmentCode = F,
  CourseID = F,
  CreatedTime = F,
  CurriculumYearID = F,
  DistrictNumber = F,
  DistrictType = F,
  EntityID = F,
  FileFolderNumber = F,
  GradeLevel = F,
  HasErrors = F,
  KeyHash = F,
  LengthOfPeriod = F,
  ModifiedTime = F,
  NumberOfPupils = F,
  PeriodsPerWeek = F,
  SchoolID = F,
  SchoolNumber = F,
  SchoolYearID = F,
  SectionID = F,
  StaffID = F,
  STARStudentSubmissionID = F,
  STARStudentSubmissionRunHistoryID = F,
  StateDistrictMNID = F,
  StateDistrictTypeCodeMNID = F,
  StateSTARAssignmentCodeMNID = F,
  StateSTARGradeLevelMNID = F,
  StateSTARModeOfTeachingMNID = F,
  TeachingMode = F,
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
\item{STARStudentFileV1ID}{The ID of the STARStudentFileV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('STARStudentFileV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

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
