% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Scheduling.R
\name{modifyTempSubstituteAssignment}
\alias{modifyTempSubstituteAssignment}
\title{Modify a TempSubstituteAssignment}
\usage{
modifyTempSubstituteAssignment(
  TempSubstituteAssignmentID,
  SubstituteStaffID = NULL,
  SourceStaffID = NULL,
  MeetID = NULL,
  CourseCodeSectionCode = NULL,
  Period = NULL,
  Date = NULL,
  HasGradebookAccess = NULL,
  HasAttendanceAccess = NULL,
  DisplayPeriodSortNumber = NULL,
  DisplayPeriodID = NULL,
  StaffMeetID = NULL,
  EntityID = NULL,
  SchoolYearID = NULL,
  Conflicts = NULL,
  SectionAlreadyCovered = NULL,
  StaffName = NULL,
  SectionID = NULL,
  IsLongTermSubstitute = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{The field values to give the modified TempSubstituteAssignment. Each defaults to NULL.}
}
\value{
The modified TempSubstituteAssignment
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a TempSubstituteAssignment
}
\concept{Scheduling}