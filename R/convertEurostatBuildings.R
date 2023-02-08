#' convertEurostatBuildings
#'
#' Convert buildings-related data from Eurostat.
#'
#' @param x raw data
#' @param subtype Eurostat code of data set
#' @returns MAgPie object with converted data
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getItems getItems<-
#' @importFrom madrat toolCountry2isocode
#' @export

convertEurostatBuildings <- function(x, subtype) {
  data <- x

  # drop EU aggregates
  data <- x[grep("^(EU|EA)", getItems(x, 1), value = TRUE), invert = TRUE, , ]

  # rename regions: ISO2 -> ISO3
  getItems(data, 1) <- gsub("^XK$", "Kosovo", getItems(data, 1))
  getItems(data, 1) <- gsub("^EL$", "Greece", getItems(data, 1))
  getItems(data, 1) <- toolCountry2isocode(getItems(data, 1))

  # fill missing regions with NA
  data <- toolCountryFill(data, verbosity = 2)

  # manually drop erroneous data points
  if (subtype == "nrg_d_hhq") {
    mselect(data, region = "ESP", period = "y2017", siec = "RA600") <- NA # nolint
  }

  return(data)
}
