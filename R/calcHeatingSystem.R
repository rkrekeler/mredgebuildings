#' Calculate installation cost or efficiency of heating systems
#'
#' @param subtype character, variable type (either 'Purchasing cost' or
#'   'Efficiency')
#' @returns MagPIE object with capacity-specific purchasing cost or efficiency
#'   of heating systems
#'
#' @author Robin Hasse
#'
#' @importFrom madrat toolGetMapping readSource toolCountryFill
#' @importFrom magclass as.magpie getSets<- setNames mbind dimSums
#'   time_interpolate
#' @importFrom utils read.csv
#' @importFrom quitte as.quitte inline.data.frame
#' @importFrom dplyr %>% right_join .data filter group_by across all_of
#'   summarise select
#' @importFrom tidyr pivot_longer
#' @export

calcHeatingSystem <- function(subtype = c("Purchasing cost", "Efficiency")) {

  subtype <- match.arg(subtype)

  # map heating technologies
  hsMap <- toolGetMapping("technologyMapping_EU_ReferenceScenario.csv",
                          type = "sectoral", where = "mredgebuildings",
                          returnPathOnly = TRUE) %>%
    read.csv(comment.char = "") %>%
    select(-"comment") %>%
    pivot_longer(matches("^weight"), names_to = "typ", values_to = "weight") %>%
    mutate(typ = sub("^weight(.*)$", "\\1", .data[["typ"]]))

  # map building types to subsectors
  typMap <- inline.data.frame(
    "typ; subsector;   FLOW",
    "SFH; Residential; RESIDENT",
    "MFH; Residential; RESIDENT",
    "Com; Services;    COMMPUB"
  )

  # map periods: assume exogenous learning
  periodMap <- inline.data.frame(
    "period; pointintime",
    "2020;   Current",
    "2030;   2030",
    "2050;   Ultimate"
  )

  data <- readSource("EU_ReferenceScenario", "techAssump.Domestic") %>%
    as.quitte(na.rm = TRUE) %>%
    select(-"period") %>%
    filter(.data[["variable"]] == subtype,
           .data[["level"]] %in% c("Current", "central")) %>%
    left_join(typMap, by = "subsector", relationship = "many-to-many") %>%
    left_join(periodMap, by = "pointintime") %>%
    left_join(hsMap, by = c("typ", "technology")) %>%
    select("region", "period", hs = "technologyBRICK", "typ", "unit", "value",
           "weight") %>%
    filter(!is.na(.data[["hs"]])) %>%
    group_by(across(-all_of(c("value", "weight")))) %>%
    summarise(value = sum(proportions(.data[["weight"]]) * .data[["value"]]),
              .groups = "drop")

  # TODO: find data for biom, libo, reel and sobo in Com # nolint: todo_comment_linter

  switch(subtype,
    `Purchasing cost` = {
      # unit conversion EUR/kW -> USD/kW
      usd2eur <- usd2eur()
      data <- data %>%
        mutate(value = .data[["value"]] / usd2eur)
      unit <- "USD2020/kW"
    },
    `Efficiency` = {
      unit <- "1"
    }
  )
  data[["unit"]] <- NULL

  # convert to magpie object
  data <- data %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value") %>%
    toolCountryFill(NA, verbosity = 2)

  # fill missing non-European countries with average
  data <- toolCountryFillAvg(data, verbosity = 2)

  # weight: FE demand
  feBuildings <- calcOutput("FEdemandBuildings", aggregate = FALSE) %>%
    time_interpolate(getItems(data, 2), extrapolation_type = "constant")

  return(list(x = data,
              unit = unit,
              weight = feBuildings,
              min = 0,
              description = subtype))
}
