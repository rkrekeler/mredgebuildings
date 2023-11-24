#' Read EHI Heating Market Report
#'
#' @param subtype Character, report to read
#' @returns magpie object
#'
#' @author Robin Hasse
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>%
#' @importFrom magclass as.magpie
#' @export

readEHI <- function(subtype) {

  file <- switch(subtype,
    `2021`    = "heatingMarketReport2021.csv",
    stop("Invalid subtype. The report '", subtype, "' is not available.")
  )

  read.csv(file) %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value")
}
