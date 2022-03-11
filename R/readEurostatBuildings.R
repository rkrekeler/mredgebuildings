#' Read Eurostat data on buildings
#'
#' Read various data sets from Eurostat that are used in the modelling of
#' buildings
#' - nrg_inf_hptc: Heat pumps - technical characteristics by technologies
#'
#' @param subtype Eurostat code of data set
#' @returns MAgPIE object with data
#'
#' @author Robin Krekeler
#'
#' @importFrom madrat toolSubtypeSelect
#' @importFrom dplyr %>% select rename
#' @importFrom utils read.csv
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

readEurostatBuildings <- function(subtype) {

  # pick file
  files <- list(
    nrg_inf_hptc = "nrg_inf_hptc__custom_2211098_linear.csv")

  data <- toolSubtypeSelect(subtype, files) %>%
    read.csv() %>%
    select(-"DATAFLOW", -"LAST.UPDATE", -"freq", -"OBS_FLAG", -"unit") %>%
    rename(region = "geo",
           period = "TIME_PERIOD",
           value = "OBS_VALUE") %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
