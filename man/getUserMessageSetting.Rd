% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MessageCenter.R
\name{getUserMessageSetting}
\alias{getUserMessageSetting}
\title{Get an UserMessageSetting}
\usage{
getUserMessageSetting(
  UserMessageSettingID,
  AssignmentScoreHighNotification = F,
  AssignmentScoreHighNotificationEmail = F,
  AssignmentScoreLowNotification = F,
  AssignmentScoreLowNotificationEmail = F,
  CopyAttendanceMessagesToEmail = F,
  CopyDisciplineMessagesToEmail = F,
  CopyEnrollmentMessagesToEmail = F,
  CopyFamilyAccessMessagesToEmail = F,
  CopyFeeManagementMessagesToEmail = F,
  CopyFoodServiceMessagesToEmail = F,
  CopyGradebookMessagesToEmail = F,
  CopyGradingMessagesToEmail = F,
  CopyGraduationRequirementsMessagesToEmail = F,
  CopyMessagesToEmail = F,
  CopyMTSSMessagesToEmail = F,
  CopyOnlineFormMessagesToEmail = F,
  CopyReportingMessagesToEmail = F,
  CopySchedulingMessagesToEmail = F,
  CreatedTime = F,
  CurrentGradeScoreHighNotification = F,
  CurrentGradeScoreHighNotificationEmail = F,
  CurrentGradeScoreLowNotification = F,
  CurrentGradeScoreLowNotificationEmail = F,
  EnableCompletedCareerPlanChangeNotification = F,
  EnableCompletedCareerPlanChangeNotificationEmail = F,
  EnableCompletedGradeChangeNotification = F,
  EnableCompletedGradeChangeNotificationEmail = F,
  EnableGradebookGradeChangeRequestDeniedEmail = F,
  EnableGradebookGradeChangeRequestNotificationEmail = F,
  EnableGradebookLastEntryNotificationEmail = F,
  EnableOnlineAssignmentAvailableNotificationEmail = F,
  EnableOnlineAssingmentScoresAvailableNotificationEmail = F,
  EnableStudentScheduleChangeNotification = F,
  EnableStudentScheduleChangeNotificationEmail = F,
  GradebookHighAssignmentThreshold = F,
  GradebookHighThreshold = F,
  GradebookLowAssignmentThreshold = F,
  GradebookLowThreshold = F,
  MissingAssignmentNotification = F,
  MissingAssignmentNotificationEmail = F,
  ModifiedTime = F,
  OnlySendAssignmentScoreHighNotificationsOncePerAssignment = F,
  OnlySendAssignmentScoreLowNotificationsOncePerAssignment = F,
  OnlySendCurrentGradeScoreHighNotificationsOnce = F,
  OnlySendCurrentGradeScoreLowNotificationsOnce = F,
  OnlySendMissingAssignmentNotificationForCurrentGradingPeriod = F,
  OnlySendMissingAssignmentNotificationsOncePerAssignment = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  UserIDOwner = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{UserMessageSettingID}{The ID of the UserMessageSetting to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserMessageSetting') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given UserMessageSetting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserMessageSetting.}
}
\value{
A dataframe or of UserMessageSetting
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of an UserMessageSetting
}
\concept{MessageCenter}
