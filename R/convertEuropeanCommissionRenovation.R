#' convertEuropeanCommissionRenovation
#'
#' Drop EU28 aggregate and rename regions to ISO3.
#'
#' @param x MAgPIE object with data from EC report
#' @return clean MAgPIE object with data from EU Buildings Database
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getItems getItems<-
#' @importFrom madrat toolCountryFill toolCountry2isocode
#'
#' @export

convertEuropeanCommissionRenovation <- function(x) {

  data <- x

  # drop EU28
  data <- data["EU28", , , invert = TRUE]

  # rename regions to ISO3
  getItems(data, 1) <- toolCountry2isocode(getItems(data, 1))

  # fill missing regions with NA
  data <- toolCountryFill(data, verbosity = 2)

  return(data)
}
