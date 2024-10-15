#' convertIEAResidentialTechnologies
#'
#' Convert residential technology data from IEA
#'
#' @author Robin Hasse
#'
#' @param x MAgPIE object with data from IEA website
#' @return clean MAgPIE object
#'
#' @importFrom magclass getItems getItems<- getNames
#' @importFrom madrat toolCountry2isocode toolCountryFill
#'
#' @export

convertIEAResidentialTechnologies <- function(x) {

  data <- x

  # rename regions: Country name -> ISO3
  getItems(data, 1) <- toolCountry2isocode(getItems(data, 1))

  # fill missing regions with mean value
  avg <- colMeans(data, TRUE, 1)
  data <- toolCountryFill(data, verbosity = 2)
  for (i in getNames(data)) {
    data[, , i][is.na(data[, , i])] <- avg[, , i]
  }

  return(data)
}
