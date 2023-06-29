#' Convert European district heating prices
#'
#' We should consider prper extrapolation
#'
#' @author Robin Hasse
#'
#'@param x MAgPIE object with original district heating prices
#'
#' @importFrom madrat toolCountry2isocode toolCountryFill
#' @importFrom magclass getItems<- getItems collapseDim
#' @export

convertEnergiforsk2016 <- function(x) {

  data <- x

  # rename regions: ISO2 -> ISO3
  getItems(data, 1) <- toolCountry2isocode(getItems(data, 1))

  # fill missing countries with NA
  data <- data %>%
    collapseDim() %>%
    toolCountryFill(verbosity = 2)

  return(data)
}
