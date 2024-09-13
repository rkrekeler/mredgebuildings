#' IEA's Tracking clean energy progress 2014
#'
#' @param subtype character, type of data
#' @author Robin Hasse
#'
#' @importFrom madrat readSource
#' @export

calcTCEP <- function(subtype) {
  return(list(x = readSource("TCEP", subtype, convert = FALSE),
              min = 0,
              isocountries = FALSE,
              unit = "m2/cap",
              description = "Floor space per capita"))
}
