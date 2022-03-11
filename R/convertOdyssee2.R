#' convertOdyssee
#'
#' Rename regions
#'
#' @param subtype category
#' @param x MAgPIE object with data from Odyssee Database
#' @return clean MAgPIE object
#'
#' @author Robin Krekeler
#'
#' @importFrom magclass getItems getItems<-
#' @importFrom madrat toolCountry2isocode toolCountryFill
#'
#' @export

convertOdyssee2 <- function(x, subtype = "households") {

  data <- x

  # rename regions: ISO2 -> ISO3
  getItems(data, 1) <- toolCountry2isocode(getItems(data, 1))

  # fill missing regions with NA
  data <- toolCountryFill(data, verbosity = 2)

  return(data)
}
