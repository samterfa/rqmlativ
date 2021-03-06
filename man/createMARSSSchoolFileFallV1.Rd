% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{createMARSSSchoolFileFallV1}
\alias{createMARSSSchoolFileFallV1}
\title{Create a MARSSSchoolFileFallV1}
\usage{
createMARSSSchoolFileFallV1(
  DistrictCode = NULL,
  DistrictType = NULL,
  EntityID = NULL,
  EntitySchoolID = NULL,
  GradeLevelCode = NULL,
  GradeLevelID = NULL,
  GradeReferenceID = NULL,
  InstructionalDays = NULL,
  IsCharter = NULL,
  KindergartenIndicator = NULL,
  MARSSSubmissionID = NULL,
  MARSSSubmissionRunHistoryID = NULL,
  MinutesInSchoolDay = NULL,
  SchoolID = NULL,
  SchoolNumber = NULL,
  SchoolYearID = NULL,
  StateDistrictMNID = NULL,
  StateDistrictTypeCodeMNID = NULL,
  StateKindergartenScheduleIndicatorCodeMNID = NULL,
  StateTitleISchoolIndicatorCodeMNID = NULL,
  TitleIIndicator = NULL,
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

\item{fieldNames}{The field values to give the created MARSSSchoolFileFallV1. Each defaults to NULL.}
}
\value{
A newly created MARSSSchoolFileFallV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates a MARSSSchoolFileFallV1
}
\concept{StateReportingMN}
