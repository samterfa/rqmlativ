% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/State Reporting MN.R
\name{listMCCCStudentCourseV1s}
\alias{listMCCCStudentCourseV1s}
\title{List MCCCStudentCourseV1s}
\usage{
listMCCCStudentCourseV1s(
  searchConditionsList = NULL,
  MCCCStudentCourseV1ID = F,
  LocalCourseCode = F,
  CourseID = F,
  SectionID = F,
  SchoolID = F,
  SectionCode = F,
  CurriculumYearID = F,
  StateCourseCodeMNID = F,
  StateCourseCode = F,
  StudentID = F,
  LastName = F,
  FirstName = F,
  MiddleName = F,
  Suffix = F,
  Gender = F,
  BirthDate = F,
  StudentStateID = F,
  MeetID = F,
  SchoolNumber = F,
  RecordType = F,
  AcademicYearNumber = F,
  MCCCAcademicYearImportID = F,
  CalendarNumber = F,
  MCCCCalendarImportID = F,
  TermType = F,
  MCCCTermImportID = F,
  TermNumber = F,
  StateSubjectAreaCodeMNID = F,
  SubjectArea = F,
  MCCCTermImportIDMarkingTerm = F,
  MarkingTermType = F,
  MarkingTermNumber = F,
  StaffID = F,
  FileFolderNumber = F,
  StudentGradeBucketID = F,
  GradeMarkIDEarned = F,
  GradeMark = F,
  LocalCreditsEarned = F,
  CollegeMarkEarned = F,
  CollegeCredits = F,
  CourseLevelMNID = F,
  CollegeCode = F,
  CollegeCourseCode = F,
  CollegeCourseTitle = F,
  MCCCSubmissionID = F,
  MCCCSubmissionRunHistoryID = F,
  EntityID = F,
  SchoolYearID = F,
  HasErrors = F,
  UserIDCreatedBy = F,
  CreatedTime = F,
  UserIDModifiedBy = F,
  ModifiedTime = F,
  InstitutionID = F,
  StateGradeMarkMNID = F,
  KeyHash = F,
  UpdateHash = F,
  EarlyEducationInstructionalMinutes = F,
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

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('MCCCStudentCourseV1') to get more field paths.}

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
A list of MCCCStudentCourseV1s
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of MCCCStudentCourseV1s
}
\concept{State Reporting MN}