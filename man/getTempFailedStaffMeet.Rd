% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Scheduling.R
\name{getTempFailedStaffMeet}
\alias{getTempFailedStaffMeet}
\title{Get a TempFailedStaffMeet}
\usage{
getTempFailedStaffMeet(
  TempFailedStaffMeetID,
  TempStaffMeetID = F,
  ErrorCount = F,
  HasErrors = F,
  StaffMeetID = F,
  MeetID = F,
  SectionID = F,
  CourseCode = F,
  SectionCode = F,
  CourseDescription = F,
  EffectiveStartDate = F,
  EffectiveEndDate = F,
  HasAttendanceAccess = F,
  HasGradebookAccess = F,
  IsPrimary = F,
  IsChecked = F,
  IsSubstitute = F,
  IsLongTermSubstitute = F,
  StaffID = F,
  StaffFullNameFML = F,
  NewEffectiveStartDate = F,
  NewEffectiveEndDate = F,
  NewStaffID = F,
  NewStaffFullNameFML = F,
  WorkflowAction = F,
  Note = F,
  Conflicts = F,
  HasConflicts = F,
  UserIDCreatedBy = F,
  CreatedTime = F,
  UserIDModifiedBy = F,
  ModifiedTime = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{TempFailedStaffMeetID}{The ID of the TempFailedStaffMeet to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFailedStaffMeet') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given TempFailedStaffMeet. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFailedStaffMeet.}
}
\value{
A dataframe or of TempFailedStaffMeet
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a TempFailedStaffMeet
}
\concept{Scheduling}