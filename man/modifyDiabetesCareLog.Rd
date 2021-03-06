% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Health.R
\name{modifyDiabetesCareLog}
\alias{modifyDiabetesCareLog}
\title{Modify a DiabetesCareLog}
\usage{
modifyDiabetesCareLog(
  DiabetesCareLogID,
  BloodGlucose = NULL,
  BloodGlucoseInsulin = NULL,
  BloodGlucoseNotChecked = NULL,
  BodyPartID = NULL,
  CarbIntake = NULL,
  DiabetesKetoneResultID = NULL,
  DistrictID = NULL,
  FoodInsulin = NULL,
  InsulinDeliveryType = NULL,
  InsulinDose = NULL,
  InsulinOnBoard = NULL,
  IsVoid = NULL,
  MedicationDosageUnitID = NULL,
  NameID = NULL,
  NameOfficeVisitID = NULL,
  NotificationMethodID = NULL,
  ParentNotified = NULL,
  RecheckBloodGlucose = NULL,
  RecheckTime = NULL,
  SchoolYearID = NULL,
  ScreeningTime = NULL,
  UserIDVoidedBy = NULL,
  VoidedTime = NULL,
  VoidNote = NULL,
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

\item{fieldNames}{The field values to give the modified DiabetesCareLog. Each defaults to NULL.}
}
\value{
The modified DiabetesCareLog
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a DiabetesCareLog
}
\concept{Health}
