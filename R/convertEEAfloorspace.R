#' convert EEAfloorpace
#'
#' Convert historic floor space data from the European Environment Agency.
#'
#' @param x MAgPIE object with floor space data
#' @return floor national floor space in million m2
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getItems getItems<- dimSums
#' @importFrom utils head
#' @importFrom madrat toolCountry2isocode toolCountryFill
#' @export

convertEEAfloorspace <- function(x) {

  # thousand m2 -> million m2
  floor <- x / 1000

  # drop summed values
  floor <- floor[head(getItems(floor, 1), -1), , ]

  # drop regions without values
  regs <- getItems(floor, 1)[which(dimSums(is.na(floor), 2) < dim(floor)[2])]
  floor <- floor[regs, , ]

  # country names to ISO3 code
  getItems(floor, 1) <- toolCountry2isocode(getItems(floor, 1))
  floor <- toolCountryFill(floor, NA, verbosity = 2)

  return(floor)
}
