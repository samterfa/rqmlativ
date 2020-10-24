% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Grading.R
\name{getElectronicTranscriptAcademicSessionV1}
\alias{getElectronicTranscriptAcademicSessionV1}
\title{Get an ElectronicTranscriptAcademicSessionV1}
\usage{
getElectronicTranscriptAcademicSessionV1(
  ElectronicTranscriptAcademicSessionV1ID,
  SessionDesignator = F,
  SectionLengthID = F,
  SessionName = F,
  SessionType = F,
  SessionSchoolYear = F,
  StudentLevel = F,
  DaysPresent = F,
  DaysAbsent = F,
  ElectronicTranscriptRunHistoryID = F,
  EntityID = F,
  SchoolYearID = F,
  StudentID = F,
  HasErrors = F,
  KeyHash = F,
  UpdateHash = F,
  UserIDCreatedBy = F,
  CreatedTime = F,
  UserIDModifiedBy = F,
  ModifiedTime = F,
  EntryWithdrawalID = F,
  OrganizationName = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{ElectronicTranscriptAcademicSessionV1ID}{The ID of the ElectronicTranscriptAcademicSessionV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ElectronicTranscriptAcademicSessionV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given ElectronicTranscriptAcademicSessionV1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ElectronicTranscriptAcademicSessionV1.}
}
\value{
A dataframe or of ElectronicTranscriptAcademicSessionV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of an ElectronicTranscriptAcademicSessionV1
}
\concept{Grading}