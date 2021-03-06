% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Curriculum.R
\name{createAcademicStandardSubject}
\alias{createAcademicStandardSubject}
\title{Create an AcademicStandardSubject}
\usage{
createAcademicStandardSubject(
  AcademicStandardSetID = NULL,
  AcademicStandardSubjectDefaultID = NULL,
  Code = NULL,
  Description = NULL,
  DistrictGroupKey = NULL,
  EnteredByDistrict = NULL,
  Key = NULL,
  Sequence = NULL,
  Year = NULL,
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

\item{fieldNames}{The field values to give the created AcademicStandardSubject. Each defaults to NULL.}
}
\value{
A newly created AcademicStandardSubject
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates an AcademicStandardSubject
}
\concept{Curriculum}
