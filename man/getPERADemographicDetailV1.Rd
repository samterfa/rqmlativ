% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingMN.R
\name{getPERADemographicDetailV1}
\alias{getPERADemographicDetailV1}
\title{Get a PERADemographicDetailV1}
\usage{
getPERADemographicDetailV1(
  PERADemographicDetailV1ID,
  PERAExtractRunID = F,
  EmployeeID = F,
  DistrictID = F,
  EmployerNumber = F,
  StateRetirementAssociationTypeMNIDPlan = F,
  StateRetirementAssociationTypeMNPlanValue = F,
  SocialSecurityNumber = F,
  LastName = F,
  FirstName = F,
  MiddleInitial = F,
  TitleFollowingName = F,
  MostRecentHireDate = F,
  EligibilityDate = F,
  StatePERAExclusionCodeMNID = F,
  StatePERAExclusionCodeMNValue = F,
  StatePERAMemberEmploymentStatusMNID = F,
  StatePERAMemberEmploymentStatusMNValue = F,
  MemberEmploymentStatusEffectiveDate = F,
  StatePERAPositionCodeMNID = F,
  StatePERAPositionCodeMNValue = F,
  StatePERAPositionClassMNID = F,
  StatePERAPositionClassMNValue = F,
  JobTitle = F,
  BirthLastName = F,
  Sex = F,
  BirthDate = F,
  AddressID = F,
  AddressAttentionTo = F,
  AddressLine1 = F,
  AddressLine2 = F,
  City = F,
  State = F,
  ZipCode = F,
  ZipCodePlusFour = F,
  ExcludeFromExport = F,
  UserIDCreatedBy = F,
  CreatedTime = F,
  UserIDModifiedBy = F,
  ModifiedTime = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{PERADemographicDetailV1ID}{The ID of the PERADemographicDetailV1 to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PERADemographicDetailV1') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given PERADemographicDetailV1. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PERADemographicDetailV1.}
}
\value{
A dataframe or of PERADemographicDetailV1
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a PERADemographicDetailV1
}
\concept{StateReportingMN}