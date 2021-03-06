% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OneRoster.R
\name{getClass}
\alias{getClass}
\title{Get a Class}
\usage{
getClass(
  ClassID,
  ClassCode = F,
  ClassType = F,
  Course = F,
  CourseSourcedID = F,
  DateLastModified = F,
  Grades = F,
  Location = F,
  ModifiedTime = F,
  OneRosterID = F,
  Periods = F,
  Resources = F,
  School = F,
  SchoolSourcedID = F,
  SectionID = F,
  SourcedID = F,
  Status = F,
  SubjectCodes = F,
  Subjects = F,
  Terms = F,
  TermSourcedIds = F,
  Title = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{ClassID}{The ID of the Class to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Class') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given Class. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Class.}
}
\value{
A dataframe or of Class
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a Class
}
\concept{OneRoster}
