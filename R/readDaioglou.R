#' Household data from Daioglou et al. 2012
#'
#' Global data on population, household size, floor space per capita (m2/cap),
#' population density (cap/km2), GINI and household expenditures (PPP2005/cap)
#' from various sources. Quintile 0 stands for the entire population.
#'
#' @author Robin Krekeler, Antoine Levesque
#'
#' @reference https://doi.org/10.1016/j.energy.2011.10.044
#'
#' @param subtype data type
#'
#' @importFrom utils read.csv
#' @importFrom dplyr rename select mutate group_by summarise across %>%
#' @importFrom rlang .data
#' @importFrom tidyr replace_na
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie collapseDim
#' @export

readDaioglou <- function(subtype = "specific floor space") {

  asNum <- function(string) {
    as.numeric(gsub(",", "", string))
  }

  data <- read.csv("Household_Characteristics3.csv",
                   stringsAsFactors = FALSE,
                   row.names = NULL) %>%
    select("region" = "Country",
           "period" = "Year",
           "demographic" = "Demographic",
           "quintile" = "Quintile",
           "population" = "Population",
           "household size" = "HHsize.cap.HH",
           "specific floor space" = "Floorspace.cap.m.2.cap",
           "population density" = "Population.Density.cap.km.2",
           "gini" = "GINI",
           "household expenditure" = "Household.Expenditure.PPP.2005.Cap",
           "source" = "Sources") %>%
    mutate("quintile" = replace_na(.data[["quintile"]], 0),
           "population" = asNum(.data[["population"]]),
           "population density" = asNum(.data[["population density"]]),
           "household expenditure" = asNum(.data[["household expenditure"]]),
           "source" = gsub("\\.", "", .data[["source"]])) %>%
    gather("variable", "value",
           -"region", -"period", -"demographic", -"quintile", -"source") %>%
    filter(!is.na(.data[["value"]]))

  if (subtype %in% unique(data[["variable"]])) {
    data <- data %>%
      filter(.data[["variable"]] == subtype)
  } else if (subtype != "all") {
    stop(paste("Subtype unavailable. Valid subtypes are: 'all',",
               paste(paste0("'", unique(data[["variable"]]), "'"),
                     collapse = ", ")))
  }

  data <- data %>%
    group_by(across(c(-"value"))) %>%
    summarise(value = mean(.data[["value"]]), .groups = "drop") %>%
    as.quitte() %>%
    as.magpie() %>%
    collapseDim()

  return(data)
}
