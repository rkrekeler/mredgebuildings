#' convertUNHouseholds
#'
#' Clean up UN data on household size and composition
#'
#' @param subtype household variable
#' @param x MAgPIE object with data from EU Buildings Database
#' @return clean MAgPIE object with
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getItems getItems<- add_dimension getSets<-
#' @madrat toolCountryFill
#' @importFrom dplyr %>% filter mutate
#' @importFrom rlang .data
#' @importFrom quitte as.quitte replace_column
#'
#' @export

convertUNHouseholds <- function(x, subtype) {

  data <- x

  # ISO3 code
  getItems(data, 1) <- toolCountry2isocode(getItems(data, 1))


  return(data)
}
