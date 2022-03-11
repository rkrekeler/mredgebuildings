#' calculate the global historic building stock
#'
#' Join data from various sources to construct a consisten global building stock
#' with a focus on floor space (million m2).
#'
#' @returns MAgPIE object with historic building stock
#'
#' @author Robin Krekeler
#'
#' @importFrom madrat readSource
#' @importFrom dplyr %>% filter
#' @importFrom quitte as.quitte
#' @importFrom rlang .data
#'
#' @export

calcBuildingStock <- function() {

  # Odyssee database (EU+)
  odyssee <- readSource("Odyssee2") %>%
    as.quitte() %>%
    filter(!is.na(.data[["value"]]))

  # EU Buildings Database
  hhsize <- readSource("EUBuildingsDB",
                       "BuildingStockCharacteristics.shareHouseholdSize") %>%
    as.quitte() %>%
    filter(!is.na(.data[["value"]]))

}
