% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Grading.R
\name{getGradeReportRunHistory}
\alias{getGradeReportRunHistory}
\title{Get a GradeReportRunHistory}
\usage{
getGradeReportRunHistory(
  GradeReportRunHistoryID,
  AddressLine1 = F,
  AddressLine2 = F,
  CEEBACT = F,
  City = F,
  Code = F,
  CreatedTime = F,
  EntityID = F,
  FamilyPrintType = F,
  FaxNumber = F,
  FooterMessage = F,
  FormattedFullAddress = F,
  GradeReportTemplateID = F,
  IsTexasTranscript = F,
  ModifiedTime = F,
  Name = F,
  OverwriteExistingReportCard = F,
  ParameterDescription = F,
  PhoneNumber = F,
  PostalCode = F,
  PostReportCardToFASA = F,
  PrintCompletedGradingPeriodComments = F,
  ReportCardFileName = F,
  RequireFamilyAccessElectronicSignature = F,
  StateProvince = F,
  StatusType = F,
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
\item{GradeReportRunHistoryID}{The ID of the GradeReportRunHistory to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GradeReportRunHistory') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given GradeReportRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GradeReportRunHistory.}
}
\value{
A dataframe or of GradeReportRunHistory
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a GradeReportRunHistory
}
\concept{Grading}
