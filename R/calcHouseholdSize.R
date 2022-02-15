#' calculate household size
#'
#' Number of persons per household.
#'
#' @note THE EU Buildings Database gives one share of all dwellings with 5 or
#' more persons. This share is weighted with 5 persons which can cause a
#' downward bias.
#'
#' @return MAgPIE object with household size
#'
#' @author Robin Krekeler
#'
#' @importFrom madrat readSource
#' @export

calcHouseholdSize <- function() {

  # load data
  eubdb <- readSource("EUBuildingsDB",
                      "BuildingStockCharacteristics.shareHouseholdSize")
  gdl <- readSource("GDL", "AreaDatabase.^hhsize$")

  # calculate household size
  getItems(eubdb, 3) <- gsub(">=", "", getItems(eubdb, 3))
  for (hs in as.numeric(getItems(eubdb, 3))) {
    eubdb[, , hs] <- hs * eubdb[, , hs]
  }
  eubdb <- dimSums(eubdb, 3)

  # drop subgroups
  gdl <- mselect(gdl, subgroup = "t", collapseNames = TRUE)

}
