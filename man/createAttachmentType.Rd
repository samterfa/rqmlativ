% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SkySys.R
\name{createAttachmentType}
\alias{createAttachmentType}
\title{Create an AttachmentType}
\usage{
createAttachmentType(
  Code = NULL,
  CustomizationID = NULL,
  Description = NULL,
  IncludeWithEmail = NULL,
  ObjectID = NULL,
  ShowInActivityAccess = NULL,
  ShowInAdministrativeAccess = NULL,
  ShowInEmployeeAccess = NULL,
  ShowInFamilyAccess = NULL,
  ShowInNewStudentEnrollment = NULL,
  ShowInStudentAccess = NULL,
  ShowInStudentServices = NULL,
  ShowInTeacherAccess = NULL,
  SkywardShowInActivityAccess = NULL,
  SkywardShowInAdministrativeAccess = NULL,
  SkywardShowInEmployeeAccess = NULL,
  SkywardShowInFamilyAccess = NULL,
  SkywardShowInNewStudentEnrollment = NULL,
  SkywardShowInStudentAccess = NULL,
  SkywardShowInStudentServices = NULL,
  SkywardShowInTeacherAccess = NULL,
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

\item{fieldNames}{The field values to give the created AttachmentType. Each defaults to NULL.}
}
\value{
A newly created AttachmentType
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function creates an AttachmentType
}
\concept{SkySys}
