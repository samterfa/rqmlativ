% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/State Reporting Federal.R
\name{listFederalW2KindOfEmployers}
\alias{listFederalW2KindOfEmployers}
\title{List FederalW2KindOfEmployers}
\usage{
listFederalW2KindOfEmployers(
  searchConditionsList = NULL,
  FederalW2KindOfEmployerID = F,
  SkywardID = F,
  Code = F,
  Description = F,
  CodeDescription = F,
  Year = F,
  UserIDCreatedBy = F,
  CreatedTime = F,
  UserIDModifiedBy = F,
  ModifiedTime = F,
  SkywardHash = F,
  fieldPaths = NULL,
  searchConditionsGroupType = "And",
  searchSortFieldNamesList = NULL,
  searchSortFieldNamesDescendingList = NULL,
  entityId = 1,
  schoolYearId = NULL,
  page = 1,
  pageSize = 1e+05,
  flatten = T
)
}
\arguments{
\item{searchConditionsList}{A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('FederalW2KindOfEmployer') to get more field paths.}

\item{searchConditionsGroupType}{The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.}

\item{searchSortFieldNamesList}{The list of fields sort results by. Defaults to NULL (unsorted).}

\item{searchSortFieldNamesDescendingList}{A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.}

\item{entityId}{The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).}

\item{schoolYearId}{The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).}

\item{page}{Results are paginated. The page of results to return. Default is 1.}

\item{pageSize}{Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).}

\item{flatten}{Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.}
}
\value{
A list of FederalW2KindOfEmployers
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of FederalW2KindOfEmployers
}
\concept{State Reporting Federal}