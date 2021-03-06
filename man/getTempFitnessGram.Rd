% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Student.R
\name{getTempFitnessGram}
\alias{getTempFitnessGram}
\title{Get a TempFitnessGram}
\usage{
getTempFitnessGram(
  TempFitnessGramID,
  ClassDescription = F,
  ClassEndDate = F,
  ClassID = F,
  ClassName = F,
  ClassStartDate = F,
  CourseCodeDescription = F,
  CreatedTime = F,
  HasMissingData = F,
  Message = F,
  ModifiedTime = F,
  ParentReportEmail1 = F,
  ParentReportEmail2 = F,
  SchoolID = F,
  SectionCode = F,
  StudentBirthdate = F,
  StudentFirstName = F,
  StudentGender = F,
  StudentGrade = F,
  StudentID = F,
  StudentLastName = F,
  StudentMiddleInitial = F,
  StudentPassword = F,
  StudentReportEmail = F,
  StudentSSOID = F,
  TeacherBirthDate = F,
  TeacherEmail = F,
  TeacherFirstName = F,
  TeacherID = F,
  TeacherLastName = F,
  TeacherMiddleInitial = F,
  TeacherPassword = F,
  TeacherSSOID = F,
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
\item{TempFitnessGramID}{The ID of the TempFitnessGram to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFitnessGram') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given TempFitnessGram. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFitnessGram.}
}
\value{
A dataframe or of TempFitnessGram
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a TempFitnessGram
}
\concept{Student}
