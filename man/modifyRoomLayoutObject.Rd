% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Attendance.R
\name{modifyRoomLayoutObject}
\alias{modifyRoomLayoutObject}
\title{Modify a RoomLayoutObject}
\usage{
modifyRoomLayoutObject(
  RoomLayoutObjectID,
  RoomLayoutID = NULL,
  RoomObjectID = NULL,
  Rotation = NULL,
  XLocation = NULL,
  YLocation = NULL,
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

\item{fieldNames}{The field values to give the modified RoomLayoutObject. Each defaults to NULL.}
}
\value{
The modified RoomLayoutObject
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function modifies a RoomLayoutObject
}
\concept{Attendance}
