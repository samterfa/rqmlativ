% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Scheduling.R
\name{modifyCourseEntityOfferedToSectionMeet}
\alias{modifyCourseEntityOfferedToSectionMeet}
\title{Modify a CourseEntityOfferedToSectionMeet}
\usage{
modifyCourseEntityOfferedToSectionMeet(
  CourseEntityOfferedToSectionMeetID,
  CourseEntityOfferedToSectionID = NULL,
  CourseEntityOfferedToSectionMeetIDClonedFrom = NULL,
  EntityIDOfferedTo = NULL,
  MeetID = NULL,
  RoomID = NULL,
  SchoolYearID = NULL,
  SectionID = NULL,
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

\item{fieldNames}{The field values to give the modified CourseEntityOfferedToSectionMeet. Each defaults to NULL.}
}
\value{
The modified CourseEntityOfferedToSectionMeet
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a CourseEntityOfferedToSectionMeet
}
\concept{Scheduling}
