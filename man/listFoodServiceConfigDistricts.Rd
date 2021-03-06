% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FoodService.R
\name{listFoodServiceConfigDistricts}
\alias{listFoodServiceConfigDistricts}
\title{List FoodServiceConfigDistricts}
\usage{
listFoodServiceConfigDistricts(
  searchConditionsList = NULL,
  AccountIDPrepaidLiability = F,
  BalanceImportFileName = F,
  BankAccountID = F,
  CalculateBalanceByFamily = F,
  ConfigDistrictID = F,
  CreatedTime = F,
  CreatePayor = F,
  CustomerCategoryImportFileName = F,
  DistrictID = F,
  FileDesinationIDBalanceImport = F,
  FileDestinationIDCustomerCategoryImport = F,
  HasInstructions = F,
  MediaIDOnlineApplicationInstructions = F,
  ModifiedTime = F,
  NameEmailIDOnlineApplication = F,
  NameIDOnlineApplicationReviewingOfficial = F,
  NextReceiptNumber = F,
  OnlineApplicationContactPhoneExtension = F,
  OnlineApplicationContactPhoneNumber = F,
  OnlineApplicationNonDiscriminationStatementOverride = F,
  OnlineApplicationOverrideNonDiscriminationStatement = F,
  OnlineApplicationStateWebsiteURL = F,
  OnlineApplicationStateWebsiteURLDisplayText = F,
  UseAccountingUpdate = F,
  UseConfirmationStepDuringVerification = F,
  UsePIN = F,
  UserIDCreatedBy = F,
  UserIDModifiedBy = F,
  fieldPaths = NULL,
  searchConditionsGroupType = "And",
  searchSortFieldNamesList = NULL,
  searchSortFieldNamesDescendingList = NULL,
  entityId = 1,
  query = NULL,
  page = 1,
  pageSize = 1e+05,
  flatten = T,
  returnResponse = F
)
}
\arguments{
\item{searchConditionsList}{A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).}

\item{fieldPaths}{Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServiceConfigDistrict') to get more field paths.}

\item{searchConditionsGroupType}{The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.}

\item{searchSortFieldNamesList}{The list of fields sort results by. Defaults to NULL (unsorted).}

\item{searchSortFieldNamesDescendingList}{A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.}

\item{entityId}{The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).}

\item{query}{Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.}

\item{page}{Results are paginated. The page of results to return. Default is 1.}

\item{pageSize}{Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).}

\item{flatten}{Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).}

\item{returnResponse}{Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.}

\item{fieldNames}{A TRUE or FALSE value determining whether or not to return the field for the given FoodServiceConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServiceConfigDistricts.}
}
\value{
A list of FoodServiceConfigDistricts
\href{https://help.skyward.com/}{Skyward's Knowledge Hub}
}
\description{
This function returns a dataframe or json object of FoodServiceConfigDistricts
}
\concept{FoodService}
