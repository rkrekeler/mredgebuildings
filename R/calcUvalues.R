#' Calculate U-value
#'
#' @export
calcUvalues <- function(subtype) {

  if (!subtype %in% c("EUBuildingsObservatory", "ETSAP")) {
    stop("Invalid subtype. Must be either 'EUBuildingsObservatory' or 'ETSAP'")
  }

  if (subtype == "EUBuildingsObservatory") {
    x <- readSource("EUBuildingsObservatory", convert = F)
    unit <- "W/m2C"
  } else {
    x <- readSource("ETSAP", convert = F)
    unit <- "W/m2K"
  }

  return(list(
    x = x,
    isocountries = FALSE,
    unit = unit,
    description = paste0(
      "energy efficiency value of building shell in ",
      "residential and non-residential as reported by ",
      subtype
    )
  ))
}
