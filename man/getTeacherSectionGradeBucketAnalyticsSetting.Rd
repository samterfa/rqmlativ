% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gradebook.R
\name{getTeacherSectionGradeBucketAnalyticsSetting}
\alias{getTeacherSectionGradeBucketAnalyticsSetting}
\title{Get a TeacherSectionGradeBucketAnalyticsSetting}
\usage{
getTeacherSectionGradeBucketAnalyticsSetting(
  TeacherSectionGradeBucketAnalyticsSettingID,
  AnalysisType = F,
  CalculationGroupGradeBucketID = F,
  CreatedTime = F,
  ModifiedTime = F,
  TeacherSectionSettingID = F,
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
\item{TeacherSectionGradeBucketAnalyticsSettingID}{The ID of the TeacherSectionGradeBucketAnalyticsSetting to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TeacherSectionGradeBucketAnalyticsSetting') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given TeacherSectionGradeBucketAnalyticsSetting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TeacherSectionGradeBucketAnalyticsSetting.}
}
\value{
A dataframe or of TeacherSectionGradeBucketAnalyticsSetting
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a TeacherSectionGradeBucketAnalyticsSetting
}
\concept{Gradebook}
