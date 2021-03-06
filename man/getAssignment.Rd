% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gradebook.R
\name{getAssignment}
\alias{getAssignment}
\title{Get an Assignment}
\usage{
getAssignment(
  AssignmentID,
  AcademicStandardID = F,
  AcademicStandards = F,
  ActiveStudentCount = F,
  ActiveStudentGroups = F,
  AllowAutoScore = F,
  AllStudentAssignmentsAreNotStarted = F,
  AnswerRevealTime = F,
  AssessmentID = F,
  AssignedDate = F,
  AssignmentIDClonedFrom = F,
  AssignmentIDParent = F,
  AssignmentQuestionCount = F,
  AssignmentSyncKey = F,
  AttachmentCount = F,
  AutoScore = F,
  AveragePercent = F,
  CalculatedPointsAllowedFromQuestions = F,
  CanDelete = F,
  CategoryID = F,
  ChildAssignmentCount = F,
  ClassesSyncedTo = F,
  CreatedTime = F,
  CreateStudentGroupAssignmentErrorMessage = F,
  DefaultPointsPerQuestion = F,
  Description = F,
  DisplayRestoreButton = F,
  DueDate = F,
  DueDateInCurrentOpenExtendedGradingPeriod = F,
  DueDateInTransactionForGivenStudentSection = F,
  DueDateIsInGivenDateRange = F,
  EffectiveAcademicStandardID = F,
  EndTime = F,
  HasParent = F,
  HasQuestions = F,
  HasStudentAssignments = F,
  HasStudentAssignmentsWithScoreOrGradeMark = F,
  HasStudentFamilyAttachmentsToDisplay = F,
  HasStudentGroupAssignments = F,
  HasUngradedStudentAssignments = F,
  HasWholeNumberWeight = F,
  HideScoreUntilTime = F,
  Instructions = F,
  IsActiveOrUnlocked = F,
  IsAHistoricRecord = F,
  IsAvailableForStudentGroup = F,
  IsAvailableForStudentSection = F,
  IsDeleted = F,
  IsOnlineAssignment = F,
  IsParent = F,
  IsPastEndTime = F,
  IsSynced = F,
  MaxScore = F,
  ModifiedTime = F,
  Name = F,
  QuestionCount = F,
  RandomizeQuestions = F,
  RelatedAssignmentsHaveAcademicStandards = F,
  RelatedAssignmentsHaveSubjects = F,
  ScoreDisplayType = F,
  ScoredStudentAssignmentCount = F,
  SectionID = F,
  SendNotificationWhenAnswersRevealed = F,
  SendNotificationWhenAssignmentReadyToTake = F,
  ShowCorrectAnswers = F,
  ShowScore = F,
  StartTime = F,
  StudentFamilyAccessAttachmentCount = F,
  SubjectID = F,
  Subjects = F,
  UseGradeMarkScoring = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  Weight = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{AssignmentID}{The ID of the Assignment to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Assignment') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given Assignment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Assignment.}
}
\value{
A dataframe or of Assignment
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of an Assignment
}
\concept{Gradebook}
