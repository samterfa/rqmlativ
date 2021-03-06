% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateReportingFederal.R
\name{getW2CSummaryView}
\alias{getW2CSummaryView}
\title{Get a W2CSummaryView}
\usage{
getW2CSummaryView(
  W2CSummaryViewID,
  CorrectBox10DependentCareBenefitsTotal = F,
  CorrectBox11NonQualifiedPlansNonSection457bTotal = F,
  CorrectBox11NonQualifiedPlansSection457bTotal = F,
  CorrectBox12AATotal = F,
  CorrectBox12BBTotal = F,
  CorrectBox12CTotal = F,
  CorrectBox12DDTotal = F,
  CorrectBox12DTotal = F,
  CorrectBox12ETotal = F,
  CorrectBox12ffTotal = F,
  CorrectBox12FTotal = F,
  CorrectBox12GTotal = F,
  CorrectBox12HTotal = F,
  CorrectBox12VTotal = F,
  CorrectBox12WTotal = F,
  CorrectBox12YTotal = F,
  CorrectBox1FederalWagesTotal = F,
  CorrectBox2FederalIncomeTaxWithheldTotal = F,
  CorrectBox3SocialSecurityWagesTotal = F,
  CorrectBox4SocialSecurityTaxWithheldTotal = F,
  CorrectBox5MedicareWagesTotal = F,
  CorrectBox6MedicareTaxWithheldTotal = F,
  CorrectBox7SocialSecurityTipsTotal = F,
  OriginallyReportedBox10DependentCareBenefitsTotal = F,
  OriginallyReportedBox11NonQualifiedPlansNonSection457bTotal = F,
  OriginallyReportedBox11NonQualifiedPlansSection457bTotal = F,
  OriginallyReportedBox12AATotal = F,
  OriginallyReportedBox12BBTotal = F,
  OriginallyReportedBox12CTotal = F,
  OriginallyReportedBox12DDTotal = F,
  OriginallyReportedBox12DTotal = F,
  OriginallyReportedBox12ETotal = F,
  OriginallyReportedBox12ffTotal = F,
  OriginallyReportedBox12FTotal = F,
  OriginallyReportedBox12GTotal = F,
  OriginallyReportedBox12HTotal = F,
  OriginallyReportedBox12VTotal = F,
  OriginallyReportedBox12WTotal = F,
  OriginallyReportedBox12YTotal = F,
  OriginallyReportedBox1FederalWagesTotal = F,
  OriginallyReportedBox2FederalIncomeTaxWithheldTotal = F,
  OriginallyReportedBox3SocialSecurityWagesTotal = F,
  OriginallyReportedBox4SocialSecurityTaxWithheldTotal = F,
  OriginallyReportedBox5MedicareWagesTotal = F,
  OriginallyReportedBox6MedicareTaxWithheldTotal = F,
  OriginallyReportedBox7SocialSecurityTipsTotal = F,
  W2CSubmissionID = F,
  fieldPaths = NULL,
  entityId = 1,
  query = NULL,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{W2CSummaryViewID}{The ID of the W2CSummaryView to return.}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('W2CSummaryView') to get more field paths.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{flatten}{Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given W2CSummaryView. Defaults to FALSE for all return fields which, for convenience, returns all fields for the W2CSummaryView.}
}
\value{
A dataframe or of W2CSummaryView
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of a W2CSummaryView
}
\concept{StateReportingFederal}
