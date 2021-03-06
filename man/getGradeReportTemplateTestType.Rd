% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Grading.R
\name{getGradeReportTemplateTestType}
\alias{getGradeReportTemplateTestType}
\title{Get a GradeReportTemplateTestType}
\usage{
getGradeReportTemplateTestType(
  GradeReportTemplateTestTypeID,
  CreatedTime = F,
  FieldGUIDTestColumn1 = F,
  FieldGUIDTestColumn10 = F,
  FieldGUIDTestColumn2 = F,
  FieldGUIDTestColumn3 = F,
  FieldGUIDTestColumn4 = F,
  FieldGUIDTestColumn5 = F,
  FieldGUIDTestColumn6 = F,
  FieldGUIDTestColumn7 = F,
  FieldGUIDTestColumn8 = F,
  FieldGUIDTestColumn9 = F,
  GradeReportTemplateID = F,
  GradeReportTemplateTestTypeIDClonedFrom = F,
  ModifiedTime = F,
  PrintHighestScoreOnly = F,
  SortNumber = F,
  TestCode = F,
  TestColumnHeaderOverride1 = F,
  TestColumnHeaderOverride10 = F,
  TestColumnHeaderOverride2 = F,
  TestColumnHeaderOverride3 = F,
  TestColumnHeaderOverride4 = F,
  TestColumnHeaderOverride5 = F,
  TestColumnHeaderOverride6 = F,
  TestColumnHeaderOverride7 = F,
  TestColumnHeaderOverride8 = F,
  TestColumnHeaderOverride9 = F,
  TestID = F,
  TestVersionID = F,
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
\item{GradeReportTemplateTestTypeID}{The ID of the GradeReportTemplateTestType to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GradeReportTemplateTestType') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given GradeReportTemplateTestType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GradeReportTemplateTestType.}
}
\value{
A dataframe or of GradeReportTemplateTestType
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a GradeReportTemplateTestType
}
\concept{Grading}
