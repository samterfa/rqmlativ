% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OnlineForm.R
\name{getOnlineFormTempEmergencyContact}
\alias{getOnlineFormTempEmergencyContact}
\title{Get an OnlineFormTempEmergencyContact}
\usage{
getOnlineFormTempEmergencyContact(
  OnlineFormTempEmergencyContactID,
  AddEmergencyContact = F,
  AllowStudentPickup = F,
  Comment = F,
  CreatedTime = F,
  CreateNewEmergencyContactName = F,
  DeleteEmergencyContact = F,
  DriversLicenseNumber = F,
  EmergencyContactID = F,
  FirstName = F,
  ForSecondFamily = F,
  IsAlsoGuardian = F,
  IsBusiness = F,
  IsHealthProfessionalName = F,
  LastName = F,
  MiddleName = F,
  ModifiedTime = F,
  NameID = F,
  OnScreenID = F,
  PrimaryEmailEmailAddress = F,
  PrimaryEmailEmailTypeID = F,
  PrimaryEmailNameEmailID = F,
  PrimaryEmailPreventFamilyStudentAccessUpdates = F,
  PrimaryPhoneExtension = F,
  PrimaryPhoneNamePhoneID = F,
  PrimaryPhonePhoneNumber = F,
  PrimaryPhonePhoneTypeID = F,
  PrimaryPhonePreventFamilyStudentAccessUpdates = F,
  Rank = F,
  RelationshipID = F,
  SecondEmailEmailAddress = F,
  SecondEmailEmailTypeID = F,
  SecondEmailNameEmailID = F,
  SecondEmailPreventFamilyStudentAccessUpdates = F,
  SecondPhoneExtension = F,
  SecondPhoneNamePhoneID = F,
  SecondPhonePhoneNumber = F,
  SecondPhonePhoneTypeID = F,
  SecondPhonePreventFamilyStudentAccessUpdates = F,
  TempEmergencyContactID = F,
  ThirdEmailEmailAddress = F,
  ThirdEmailEmailTypeID = F,
  ThirdEmailNameEmailID = F,
  ThirdEmailPreventFamilyStudentAccessUpdates = F,
  ThirdPhoneExtension = F,
  ThirdPhoneNamePhoneID = F,
  ThirdPhonePhoneNumber = F,
  ThirdPhonePhoneTypeID = F,
  ThirdPhonePreventFamilyStudentAccessUpdates = F,
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
\item{OnlineFormTempEmergencyContactID}{The ID of the OnlineFormTempEmergencyContact to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineFormTempEmergencyContact') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given OnlineFormTempEmergencyContact. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineFormTempEmergencyContact.}
}
\value{
A dataframe or of OnlineFormTempEmergencyContact
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of an OnlineFormTempEmergencyContact
}
\concept{OnlineForm}
