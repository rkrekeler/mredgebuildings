#' Read Odyssee data base
#'
#' Read and tidy data on final energy consumption for different end uses in
#' residential (households) and commercial (services) buildings in Europe.
#' Calculates end use or carrier shares for European EDGE regions.
#'
#' @returns data.frame with historic enduse or carrier shares
#'
#' @author Antoine Levesque, Pascal Führlich
#'
#' @references Enerdata. Odyssee-Mure database. https://www.odyssee-mure.eu/
#'
#' @importFrom madrat toolCountry2isocode
readOdyssee <- function() {
  services <- read.csv("export_enerdata_7793_105638_services.csv", skip = 1, na.strings = c("n.a.", ""))
  households <- read.csv("export_enerdata_7793_105710_households.csv", skip = 1, na.strings = c("n.a.", ""))
  odyssee <- rbind(households, services)
  colnames(odyssee) <- c("variable", "region", "unit", "year", "value", "note", "title")
  odyssee$region <- toolCountry2isocode(odyssee$region)
  odyssee$year <- paste0("y", odyssee$year)
  return(as.magpie(odyssee))
}
