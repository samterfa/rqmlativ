% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gradebook.R
\name{deleteTeacherSectionGradingPeriodSetting}
\alias{deleteTeacherSectionGradingPeriodSetting}
\title{Delete a TeacherSectionGradingPeriodSetting}
\usage{
deleteTeacherSectionGradingPeriodSetting(
  TeacherSectionGradingPeriodSettingID,
  ignoreWarnings = F,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{TeacherSectionGradingPeriodSettingID}{The ID of the TeacherSectionGradingPeriodSetting to delete}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}
}
\value{
The TeacherSectionGradingPeriodSettingID of the deleted TeacherSectionGradingPeriodSetting.
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function deletes a TeacherSectionGradingPeriodSetting
}
\concept{Gradebook}
