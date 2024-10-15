#' Read input data of Res-IRF
#'
#' Input data of the residential sector model of the IMACLIM team.
#'
#' The scope of the model is France.
#'
#' @author Robin Hasse
#'
#' @source https://doi.org/10.1016/j.eneco.2011.07.010
#' @source https://github.com/CIRED/Res-IRF/tree/v3.1/project/input
#'
#' @param subtype character, type of input data
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>% .data filter rename
#' @importFrom tidyr pivot_longer

demand <- c("G" = 596,
            "F" = 392,
            "E" = 280,
            "D" = 191,
            "C" = 125,
            "B" = 76,
            "A" = 39)

readResIRF <- function(subtype) {
  file <- switch(subtype,
    costRenovation.Giraudet2012 = "cost_renovation_Giraudet2012.csv",
    costRenovation              = "cost_renovation.csv",
    costRenovation.2012         = "cost_renovation_2012.csv",
    costRenovation.2018         = "cost_renovation_2018.csv",
    stop("Invalid subtype: '", subtype, "'")
  )

  if (grepl("^costRenovation", subtype)) {
    data <- read.csv(file) %>%
      rename(initial = 1) %>%
      pivot_longer(-1, names_to = "final", values_to = "value") %>%
      filter(!is.na(.data[["value"]]))


    data <- data %>%
      mutate(reduction = 1 - demand[final] / demand[initial]) # nolint: object_usage_linter.
  }
}
