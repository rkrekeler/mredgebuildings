#' read EU28 building stock from Hotmaps
#'
#' Consistent building stock of EU28 residential and commercial sector. The data
#' presents a snapshot, I guess in 2016 but this is not clear to e yet.
#'
#' @author Robin Hasse
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>% mutate
#' @importFrom tidyr unite
#' @importFrom rlang .data
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

readHotmaps <- function() {
  data <- read.csv("building_stock.csv", sep = "|") %>%
    group_by(across(-any_of(c("estimated", "value", "unit", "source")))) %>%
    summarise(value = mean(.data[["value"]]), .groups = "drop") %>%
    unite("variable", "topic", "feature", "type", "detail") %>%
    unite("building", "sector", "subsector", "btype") %>%
    select(region = "country_code",
           "variable",
           "building",
           "bage",
           "value") %>%
    mutate(period = 2016,
           variable = gsub("_$", "", .data[["variable"]])) %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
