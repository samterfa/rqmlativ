% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Curriculum.R
\name{getAcademicStandard}
\alias{getAcademicStandard}
\title{Get an AcademicStandard}
\usage{
getAcademicStandard(
  AcademicStandardID,
  AcademicStandardDefaultID = F,
  AcademicStandardGradeRangeID = F,
  AcademicStandardIDParent = F,
  BackgroundColor = F,
  ChildAcademicStandardCount = F,
  CreatedTime = F,
  Description = F,
  DescriptionToUse = F,
  DisplayAs = F,
  DistrictGroupKey = F,
  EnteredByDistrict = F,
  ExtendedDescription = F,
  FullKey = F,
  FullKeyPrefix = F,
  GrandChildLevelHierarchyDepthDescription = F,
  Guid = F,
  HierarchyDepthDescription = F,
  IsAttachedToASubject = F,
  IsHighFrequencyWord = F,
  IsLettersAndSounds = F,
  IsPlaceHolder = F,
  Key = F,
  Label = F,
  LetterAndSoundType = F,
  LetterType = F,
  Level = F,
  ModifiedTime = F,
  NextLevelHierarchyDepthDescription = F,
  ParentGuid = F,
  Sequence = F,
  StateNumber = F,
  TextColor = F,
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
\item{AcademicStandardID}{The ID of the AcademicStandard to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandard') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandard. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandard.}
}
\value{
A dataframe or of AcademicStandard
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of an AcademicStandard
}
\concept{Curriculum}
